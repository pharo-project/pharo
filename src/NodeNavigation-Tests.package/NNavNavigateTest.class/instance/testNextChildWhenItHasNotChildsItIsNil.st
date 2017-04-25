children
testNextChildWhenItHasNotChildsItIsNil
	| child parent |
	
	parent := RBSequenceNode new.
	
	child := NNavDirectionFirstChild childNodeFor:  parent .
	self assert: child isNil .