"
A trait for test purposes
"
Trait {
	#name : 'TPrintTest',
	#category : 'Collections-Abstract-Tests-Traits',
	#package : 'Collections-Abstract-Tests',
	#tag : 'Traits'
}

{ #category : 'tests - printing' }
TPrintTest >> elements [

	^ self nonEmpty isDictionary
		  ifTrue: [ self nonEmpty associations ]
		  ifFalse: [ self nonEmpty asOrderedCollection ]
]

{ #category : 'requirements' }
TPrintTest >> nonEmpty [
 self explicitRequirement
]

{ #category : 'tests - printing' }
TPrintTest >> prefix [

	^ self nonEmpty class name first isVowel
		  ifTrue: [ 'an' ]
		  ifFalse: [ 'a' ]
]

{ #category : 'tests - fixture' }
TPrintTest >> test0FixturePrintTest [

	self denyEmpty: self nonEmpty
]

{ #category : 'tests - printing' }
TPrintTest >> testPrintElementsOn [

	| printed printedElements allPrintedElementsAsString allElementsAsString |
	allElementsAsString := self elements collect: [ :each |
		                       String streamContents: [ :stream |
			                       each printOn: stream ] ].

	printed := String streamContents: [ :stream |
		           self elements printElementsOn: stream ].
	printedElements := printed trimBoth: [ :char | char isPunctuation ].
	allPrintedElementsAsString := printedElements findBetweenSubstrings: ' '.

	self
		assertCollection: allElementsAsString
		hasSameElements: allPrintedElementsAsString
]

{ #category : 'tests - printing' }
TPrintTest >> testPrintNameOn [

	| printed prefix expected |
	printed := String streamContents: [ :stream |
		           self nonEmpty printNameOn: stream ].
	prefix := self prefix.
	expected := prefix , ' ' , self nonEmpty class name.

	self assert: printed equals: expected
]

{ #category : 'tests - printing' }
TPrintTest >> testPrintOn [

	| printed allElementsAsString splittedString |
	allElementsAsString := self elements collect: [ :each |
		                       String streamContents: [ :stream |
			                       each printOn: stream ] ].

	printed := String streamContents: [ :stream |
		           self nonEmpty printOn: stream ].

	splittedString := printed findBetweenSubstrings: { ' '. '('. ')' }.
	self assertCollection: splittedString hasSameElements: {
			self prefix.
			self nonEmpty class name } , allElementsAsString
]

{ #category : 'tests - printing' }
TPrintTest >> testPrintOnDelimiter [

	| result allElementsAsString expectedelements |
	result := String streamContents: [ :aStream |
		          self nonEmpty printOn: aStream delimiter: ', ' ].

	allElementsAsString := result findBetweenSubstrings: ', '.
	expectedelements := self nonEmpty
		                    collect: [ :e |
		                    String streamContents: [ :aStream |
			                    e printOn: aStream ] ]
		                    as: OrderedCollection.
		
	self assert: allElementsAsString equals: expectedelements
]

{ #category : 'tests - printing' }
TPrintTest >> testPrintOnDelimiterLast [

	| result |
	result := String streamContents: [ :aStream |
		          self nonEmpty printOn: aStream delimiter: ', ' last: ' and ' ].

	self assert: (result occurrencesOf: $,) equals: self elements size - 2
]
