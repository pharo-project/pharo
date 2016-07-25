variable accessing
whoDefinesClassVariable: aString 
	| sprClass |
	(self directlyDefinesClassVariable: aString) ifTrue: [^self].
	sprClass := self superclass.
	^sprClass isNil 
		ifTrue: [nil]
		ifFalse: [sprClass whoDefinesClassVariable: aString]