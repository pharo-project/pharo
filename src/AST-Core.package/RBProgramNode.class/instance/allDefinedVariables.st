accessing
allDefinedVariables
	| children |
	children := self children.
	children isEmpty ifTrue: [^#()].
	^children inject: OrderedCollection new
		into: 
			[:vars :each | 
			vars
				addAll: each allDefinedVariables;
				yourself]