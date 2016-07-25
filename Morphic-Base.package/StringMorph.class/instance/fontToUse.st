accessing
fontToUse
	| fontToUse |
	fontToUse := font isNil ifTrue: [TextStyle defaultFont] ifFalse: [font].
	(emphasis isNil or: [emphasis = 0]) 
		ifTrue: [^fontToUse]
		ifFalse: [^fontToUse emphasized: emphasis]