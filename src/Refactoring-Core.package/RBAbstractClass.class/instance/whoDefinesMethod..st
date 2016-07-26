method accessing
whoDefinesMethod: aSelector 
	| sprClass |
	(self directlyDefinesMethod: aSelector) ifTrue: [^self].
	sprClass := self superclass.
	^sprClass isNil 
		ifTrue: [nil]
		ifFalse: [sprClass whoDefinesMethod: aSelector]