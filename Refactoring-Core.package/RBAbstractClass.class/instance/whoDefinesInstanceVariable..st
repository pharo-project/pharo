variable accessing
whoDefinesInstanceVariable: aString 
	| sprClass |
	(self directlyDefinesInstanceVariable: aString) ifTrue: [^self].
	sprClass := self superclass.
	^sprClass isNil 
		ifTrue: [nil]
		ifFalse: [sprClass whoDefinesInstanceVariable: aString]