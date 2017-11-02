Extension { #name : #Array }

{ #category : #'*GT-Inspector' }
Array >> gtInspectorFastPrintOn: aStream [
	"Only scan through a limited number of elements to determine how to print the Array."
	((1 to: (self size min: self gtCollectionSizeThreshold)) allSatisfy: [ :anIndex | 
			(self at: anIndex) shouldBePrintedAsLiteral ]) ifTrue: [
		self printAsLiteralFormOn: aStream.  ^ self].
	((1 to: (self size min: self gtCollectionSizeThreshold)) allSatisfy: [ :anIndex | 
			(self at: anIndex) isSelfEvaluating ])  ifTrue: [
		self printAsSelfEvaluatingFormOn: aStream. ^ self].

	super printOn: aStream
]
