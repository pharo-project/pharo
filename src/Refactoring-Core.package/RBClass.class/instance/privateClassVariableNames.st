private
privateClassVariableNames
	(self isDefined and: [classVariableNames isNil]) 
		ifTrue: [self classVariableNames: self realClass classVarNames].
	^classVariableNames