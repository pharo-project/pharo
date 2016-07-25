accessing
selectionIntervalFor: aString 
	| interval |
	interval := super selectionIntervalFor: aString.
	^interval notNil 
		ifTrue: [interval]
		ifFalse: [environment selectionIntervalFor: aString]