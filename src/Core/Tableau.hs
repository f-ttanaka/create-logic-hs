module Core.Tableau where

import Common
import Prop

data Tableau
  = Closed -- 矛盾が見つかった（枝が閉じている）
  | Open [Prop] -- 展開可能な式がなく、矛盾もない（開いている枝）
  | Branch [Tableau] -- 分岐（A ∨ B など）
  | Expand [Prop] Tableau -- 新たに展開した式とその結果
  deriving (Eq, Show)

type Path = [Prop]

expandOnce :: [Prop] -> Tableau
expandOnce props =
  case findContradiction props of
    True -> Closed
    False ->
      case findExpandable props of
        Nothing -> Open props
        Just (p, rest) ->
          case toNNF p of
            And a b -> Expand [p] (expandOnce (a : b : rest))
            Or a b -> Expand [p] (Branch [expandOnce (a : rest), expandOnce (b : rest)])
            _ -> Expand [p] (expandOnce (toNNF p : rest))
 where
  -- 矛盾があるかチェック
  findContradiction :: [Prop] -> Bool
  findContradiction ps = any (\p -> negationOf p `elem` ps) ps

  -- 展開すべき命題を一つ見つける（リテラルはスキップ）
  findExpandable :: [Prop] -> Maybe (Prop, [Prop])
  findExpandable [] = Nothing
  findExpandable (p : ps)
    | isLiteral p = fmap (second (p :)) (findExpandable ps)
    | otherwise = Just (p, ps)

expandFully :: Tableau -> Tableau
expandFully Closed = Closed
expandFully (Open props) =
  case expandOnce props of
    Closed -> Closed
    t@(Open _) -> t
    t -> expandFully t
expandFully (Expand ps t) = Expand ps (expandFully t)
expandFully (Branch ts) = Branch (map expandFully ts)

isSatisfiable :: [Prop] -> Bool
isSatisfiable props =
  case expandFully (expandOnce props) of
    t -> anyOpen t
 where
  anyOpen :: Tableau -> Bool
  anyOpen Closed = False
  anyOpen (Open _) = True
  anyOpen (Expand _ t) = anyOpen t
  anyOpen (Branch ts) = any anyOpen ts
