{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.Table.TH (isA, makeTable) where
import Language.Haskell.TH
import Control.Lens
import Data.Table
import Data.Monoid
import Control.Applicative
import Control.Monad


classifyType :: Name -> Name
classifyType x
    | x == 'Primary = ''Primary
    | x == 'Candidate = ''Candidate
    | x == 'CandidateInt = ''Candidate
    | x == 'Supplemental = ''Supplemental
    | x == 'SupplementalInt = ''Supplemental
    | x == 'SupplementalHash = ''Supplemental
    | x == 'Inverted = ''Inverted
    | x == 'InvertedInt = ''Inverted
    | x == 'InvertedHash = ''Inverted
classifyType x = x


-- | utility for 'makeTable'
isA :: Name -> Name -> (Name -> First Name)
isA a b x
    | a == x = First (Just b)
    | otherwise = mempty

{- | given a data type like:

data RowT t = RowC { a,b,c :: Int, d :: t }

makeTable ''RowT 'a ['b `isA` 'CandidateInt, 'c `isA` 'Supplemental]

generates an instance of 'Tabular' which has @a@ as the 'Primary' key, @b@
as a 'CandidateInt' key and @c@ as a 'Supplemental'

this works better than 'makeTabular'

-}
makeTable :: Name  -- ^ data type name @''RowT@
        -> Name -- ^ label for the primary key (@'a@)
        -> [Name -> First Name] -- ^ @f 'b@ and @f 'c@ should
                                -- name a constructor of 'KeyType'
        -> Q [Dec]
makeTable typeName idField otherFields = do
    TyConI (DataD _cxt _typeName tvs [RecC _conName fields] _derives) <- reify typeName

    let fields' = [ (n,strict,t,k')
                  | (n,strict,t) <- fields,
                    k' <- case getFirst (mconcat otherFields n) of
                        Nothing | n == idField -> [''Primary]
                        Just a -> [a]
                        _ -> [] ]
        
        ty = foldl appT (conT typeName)
            [ varT $ case tv of
                    PlainTV x -> x
                    KindedTV x _ -> x
                | tv <- tvs ]
        

        pktTy | [x] <- lookup3 idField fields = x
            | otherwise = error $ "makeTable: cannot find idField `" ++ show idField
                                    ++ "' in fields of data `" ++ show typeName
                                        ++ "' which are `" ++ show fields ++ "'"

        k = varT (mkName "k")
        i = varT (mkName "i")
        b = varT (mkName "b")


        keyCon fn = mkName (nameBase typeName ++ nameBase fn)
        tabCon = (mkName (nameBase typeName ++ "Tab"))

    fmap (:[]) $ instanceD (return []) [t| Tabular $ty |]
            [ tySynInstD ''PKT [ty] (return pktTy),
              dataInstD (return []) ''Key [ k, ty, b ]
                [forallC [] (sequence
                                ([equalP k (conT (classifyType k')),
                                  equalP b (return ty)]))
                        (normalC (keyCon fn) [])
                    | (fn,_,ty,k') <- fields']
                [],
              dataInstD (return []) ''Tab [ty, i]
                [normalC tabCon
                        [ -- not sure about importance of strictness for the Tab
                          fmap ((,) strict) [t| $i $(conT (classifyType k')) $(return fTy) |]
                        | (fn,strict,fTy,k') <- fields']
                    ]
                [],
              funD 'fetch [ clause [conP (keyCon fn) []]
                                        (normalB (varE fn))
                                        []
                          | (fn,_,_,_) <- fields'],
              valD (varP 'primary) (normalB (conE (keyCon idField))) [],
              funD 'primarily  [do
                               x <- newName "x"
                               clause [conP (keyCon idField) [], varP x]
                                    (normalB (varE x))
                                    []],
              funD 'mkTab [do
                        f <- newName "f"
                        clause [varP f]
                         (normalB
                            (foldl
                                (\x y -> [| $x <*> $(varE f) $y |])
                                [| pure $(conE tabCon) |]
                                [ conE (keyCon fn) | (fn, _, _, _) <- fields' ]
                                ))
                         []],
              funD 'forTab [do
                        f <- newName "f"
                        xs <- zipWithM const (repeat (newName "x")) fields'
                        clause [conP tabCon (map varP xs), varP f]
                          (normalB
                            (foldl
                                (\x (l,v) -> [| $x <*> $(varE f) $l $v |]) 
                                [| pure $(conE tabCon) |]
                                [  (conE (keyCon fn), varE v)
                                    | ((fn, _, _, _),v) <- fields' `zip` xs ]
                                ))
                          []
                        ],
              funD 'ixTab [do
                        x <- newName "x"
                        clause [conP tabCon [ if fn' == fn then varP x else wildP
                                            | (fn', _, _, _) <- fields' ],
                                conP (keyCon fn) []]
                            (normalB (varE x))
                            []
                      | (fn, _, _, _) <- fields' ],
              valD
                (varP 'autoTab)
                (normalB [| autoIncrement $
                                lens
                                   $(varE idField)
                                   (\s b -> $(recUpdE [|s|] [fmap ((,)idField) [|b|]]))
                                   |])
                []
                     ]


lookup3 :: Eq a => a -> [(a,b,c)] -> [c]
lookup3 n xs = [ b | (a,_,b) <- xs, n == a ]
