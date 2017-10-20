correcting
defaultAction
	| varName className selector |
	
 	className  :=  self methodClass name .
	selector := self methodNode selector. 
	varName := node name.

	self methodNode selector ifNotNil: [self crTrace: className, '>>', selector, ' ']
			ifNil: [self traceCr:''].
	self traceCr: '(' , varName , ' is Undeclared) '.

	^super defaultAction ifNil: [
		Undeclared add: (UndeclaredVariable named: varName asSymbol).
		OCUndeclaredVariable new name: varName asSymbol]