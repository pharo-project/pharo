private
privateInstanceVariableNames
	instanceVariableNames isNil 
		ifTrue: 
			[self isDefined 
				ifTrue: [self instanceVariableNames: self realClass instVarNames]
				ifFalse: [instanceVariableNames := OrderedCollection new]].
	^instanceVariableNames