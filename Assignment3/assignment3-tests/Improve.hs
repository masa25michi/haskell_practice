module Improve where
import StatementsExpressions

improve :: Statement -> Statement

improve (IfStmt (VarExp _) s) = (improve s)
improve (ExpStmt x) = (ExpStmt (improveExpression x))
improve (AssignStmt str exp) = (AssignStmt str (improveExpression exp))

improveExpression :: Expression -> Expression

improveExpression (EqualsExp exp1 exp2) = EqualsExp (improveExpression exp1) (improveExpression exp2)
improveExpression (BeginExp [] exp) = (improveExpression exp)
improveExpression (BeginExp lst exp) = BeginExp (map improve lst) (improveExpression exp)
improveExpression exp = exp
