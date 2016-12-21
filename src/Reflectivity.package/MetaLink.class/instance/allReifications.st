accessing
allReifications
	| reifications |
	"we only check reification, that is symbols."
	reifications := (self arguments select: [:each | each isSymbol]) asSet. 
	self metaObject isSymbol ifTrue: [reifications add: self metaObject].
	
	self conditionArguments ifNil: [  
		self condition ifNotNil: [reifications addAll: (self reificationsFor: self condition)]]
		ifNotNil: [reifications addAll: self conditionArguments ].
	
	^reifications