accessing
allInstanceVariableNames
	| sprClass |
	sprClass := self superclass.
	^sprClass isNil 
		ifTrue: [self instanceVariableNames]
		ifFalse: [sprClass allInstanceVariableNames , self instanceVariableNames]