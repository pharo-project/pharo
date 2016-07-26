accessing
allClassVariableNames
	| sprClass |
	sprClass := self superclass.
	^sprClass isNil 
		ifTrue: [self classVariableNames]
		ifFalse: [sprClass allClassVariableNames , self classVariableNames]