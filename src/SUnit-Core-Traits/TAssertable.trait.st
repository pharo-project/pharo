"
I think it is not used yet. I think it is a Trait so that we can make things assertable and not need to subclass from TestCase for example. I may expect that TestCase uses such Trait instead of implementing all those methods,  but it is not the case. Even more, TAssertable users size -> 0. 

So...seems a work half done. Adrian Kuhn introduce it. Check
http://forum.world.st/template/NamlServlet.jtp?macro=search_page&node=1294837&query=TAssertable
"
Trait {
	#name : #TAssertable,
	#category : #'SUnit-Core-Traits'
}

{ #category : #asserting }
TAssertable >> assert: actualNumber closeTo: expectedNumber [
	^ self
		assert: (actualNumber closeTo: expectedNumber)
		description: [self comparingStringBetween: actualNumber and: expectedNumber]
]

{ #category : #asserting }
TAssertable >> assert: aBooleanOrBlock description: aStringOrBlock [
	aBooleanOrBlock value ifFalse: [
		| message |
		message := aStringOrBlock value.
		self classForTestResult failure signal: message]
			
]

{ #category : #asserting }
TAssertable >> assert: aBooleanOrBlock description: aStringOrBlock resumable: resumableBoolean [ 
	| exception |
	aBooleanOrBlock value
		ifFalse: 
			[|aString|
			aString := aStringOrBlock value.
			exception := resumableBoolean
						ifTrue: [self classForTestResult resumableFailure]
						ifFalse: [self classForTestResult failure].
			exception signal: aString]
			
]

{ #category : #asserting }
TAssertable >> assert: actual equals: expected [
	^ self
		assert: actual = expected
		description: [self comparingStringBetween: actual and: expected]
]

{ #category : #asserting }
TAssertable >> assert: actual identicalTo: expected [
	^ self
		assert: expected == actual
		description: [self comparingIdentityStringBetween: actual and: expected]
]

{ #category : #asserting }
TAssertable >> assertCollection: actual equals: expected [
	"Specialized test method that generates a proper error message for collection"
	^ self
		assert: expected = actual
		description: [ self comparingCollectionBetween: actual and: expected ]
]

{ #category : #asserting }
TAssertable >> assertCollection: actual hasSameElements: expected [
	"Assert that a collection contains the same elements as the given collection. Order is not checked, only the presence/absence of elements."
	| missingElements additionalElements |
	
	additionalElements := actual difference: expected.
	missingElements := expected difference: (actual intersection: expected).
	self 
		assert: (additionalElements isEmpty and: [ missingElements isEmpty ])
		description: (String streamContents: 
			[:stream |
			stream
				nextPutAll: 'Given Collections do not match!'; lf;
				tab; nextPutAll: 'additions : '; print: additionalElements asArray; lf;
				tab; nextPutAll: 'missing: '; print: missingElements asArray; lf ])
]

{ #category : #asserting }
TAssertable >> assertEmpty: aCollection [
	^ self assert: aCollection isEmpty description: aCollection asString , ' should have been empty'
]

{ #category : #asserting }
TAssertable >> classForTestResult [
	"Returns the class of the test result"
	^ TestResult
]

{ #category : #private }
TAssertable >> comparingCollectionBetween: left and: right [
	| additionalLeft additionalRight sortBlock|
	
	"use a very slow sort block"
	sortBlock := [ :a :b | a asString <= b asString ].
	additionalLeft := (left difference: right) sorted: sortBlock.
	additionalRight := (right difference: left) sorted: sortBlock. 
	
	^ String streamContents: [:stream |
		stream
			nextPutAll: 'Given Collections do not match. Got '; lf;
			tab; nextPutAll: 'left := '; print: left; nextPut: $.; lf;
			nextPutAll: ' instead of ';
			tab; nextPutAll: ' right :='; print: left; nextPut: $.; lf.
		left size = right size
			ifFalse: [ 
				stream 
					nextPutAll: 'Collection size does not match: left='; 
					print: left size;
					nextPutAll: ' vs. right=';
					print: right size; lf ].
		additionalLeft isEmpty
			ifFalse: [ 
				stream 
					nextPutAll: 'Got ';
					print: additionalLeft size;
					nextPutAll: ' additional element(s) in the left collection: ';
					tab; print: additionalLeft  ].
		additionalRight isEmpty
			ifFalse: [ 
				stream 
					nextPutAll: 'Got ';
					print: additionalRight size;
					nextPutAll: ' additional element(s) in the right collection: ';
					tab; print: additionalRight  ]]
]

{ #category : #private }
TAssertable >> comparingIdentityStringBetween: actual and: expected [

	^ String streamContents: [:stream |
			stream
				nextPutAll: actual fullPrintString;
				nextPutAll: ' is not identical to ';
				nextPutAll: expected fullPrintString;
				nextPutAll: '.']
]

{ #category : #private }
TAssertable >> comparingStringBetween: actual and: expected [

	^ String streamContents: [:stream |
			stream
				nextPutAll: 'Got ';
				nextPutAll: actual fullPrintString;
				nextPutAll: ' instead of ';
				nextPutAll: expected fullPrintString;
				nextPutAll: '.']
]

{ #category : #asserting }
TAssertable >> deny: aBooleanOrBlock [

	self assert: aBooleanOrBlock value not
			
]

{ #category : #asserting }
TAssertable >> deny: aBooleanOrBlock description: aString [
	self assert: aBooleanOrBlock value not description: aString
			
]

{ #category : #asserting }
TAssertable >> deny: aBooleanOrBlock description: aString resumable: resumableBoolean [ 
	self
		assert: aBooleanOrBlock value not
		description: aString
		resumable: resumableBoolean
			
]

{ #category : #asserting }
TAssertable >> deny: actual equals: expected [
	^ self
		deny: expected = actual
		description: [self unexpectedEqualityStringBetween: actual and: expected]
]

{ #category : #asserting }
TAssertable >> deny: actual identicalTo: expected [
	^ self
		deny: expected == actual
		description: [self unexpectedIdentityEqualityStringBetween: actual and: expected]
]

{ #category : #asserting }
TAssertable >> denyCollection: actual equals: expected [
	"Specialized test method that generates a proper error message for collection"
	^ self
		deny: expected = actual
		description: [ self unexpectedEqualityStringBetween: actual and: expected ]
]

{ #category : #asserting }
TAssertable >> denyCollection: actual hasSameElements: expected [
	"Deny that a collection contains the same elements as the given collection. Order is not checked, only the presence/absence of elements."

	self
		deny: ((actual difference: expected) isEmpty and: [ (expected difference: actual) isEmpty ])
		description: 'Given collections match!'
]

{ #category : #asserting }
TAssertable >> denyEmpty: aCollection [
	^ self assert: aCollection isNotEmpty description: aCollection asString , ' should not have been empty'
]

{ #category : #asserting }
TAssertable >> executeShould: aBlock inScopeOf: anExceptionalEvent [ 
	^[aBlock value.
 	false] on: anExceptionalEvent
		do: [:ex | ex return: true]
			
]

{ #category : #asserting }
TAssertable >> executeShould: aBlock inScopeOf: anExceptionalEvent withDescriptionContaining: aString [
	^[aBlock value.
 	false] on: anExceptionalEvent
		do: [:ex | ex return: (ex description includesSubstring: aString) ]
			
]

{ #category : #asserting }
TAssertable >> executeShould: aBlock inScopeOf: anExceptionalEvent withDescriptionNotContaining: aString [
	^[aBlock value.
 	false] on: anExceptionalEvent
		do: [:ex | ex return: (ex description includesSubstring: aString) not ]
			
]

{ #category : #asserting }
TAssertable >> executeShould: aBlock inScopeOf: anException withExceptionDo: anotherBlock [

	^[aBlock value.
 	false] 
		on: anException
		do: [:exception | 
			anotherBlock value: exception.
			exception return: true]
]

{ #category : #asserting }
TAssertable >> fail [

	^self assert: false
]

{ #category : #asserting }
TAssertable >> fail: aDescriptionString [

	^self assert: false description: aDescriptionString
]

{ #category : #asserting }
TAssertable >> should: aBlock [
	self assert: aBlock value
			
]

{ #category : #asserting }
TAssertable >> should: aBlock description: aString [
	self assert: aBlock value description: aString
			
]

{ #category : #asserting }
TAssertable >> should: aBlock notTakeMoreThan: aDuration [
    "Evaluate aBlock and if it takes more than given duration
    to run we report a test failure. "

	^ aBlock valueWithin: aDuration onTimeout: [
		self 
			assert: false 
			description: ['Block evaluation took more than the expected <1p>' expandMacrosWith: aDuration]
	]
	

]

{ #category : #asserting }
TAssertable >> should: aBlock notTakeMoreThanMilliseconds: anInteger [
    "For compatibility with other Smalltalks"

   self should: aBlock notTakeMoreThan: (Duration milliSeconds: anInteger).
]

{ #category : #asserting }
TAssertable >> should: aBlock raise: anExceptionalEvent [ 
	^self assert: (self executeShould: aBlock inScopeOf: anExceptionalEvent)
			
]

{ #category : #asserting }
TAssertable >> should: aBlock raise: anExceptionalEvent description: aString [ 
	^self assert: (self executeShould: aBlock inScopeOf: anExceptionalEvent)
		description: aString
			
]

{ #category : #asserting }
TAssertable >> should: aBlock raise: anExceptionalEvent whoseDescriptionDoesNotInclude: substring description: aString [ 
	^self assert: (self executeShould: aBlock inScopeOf: anExceptionalEvent withDescriptionNotContaining: substring)
		description: aString

]

{ #category : #asserting }
TAssertable >> should: aBlock raise: anExceptionalEvent whoseDescriptionIncludes: substring description: aString [ 
	^self assert: (self executeShould: aBlock inScopeOf: anExceptionalEvent withDescriptionContaining: substring)
		description: aString

]

{ #category : #asserting }
TAssertable >> should: aBlock raise: anException withExceptionDo: anotherBlock [ 

	^self assert: (self executeShould: aBlock inScopeOf: anException withExceptionDo: anotherBlock)
]

{ #category : #asserting }
TAssertable >> shouldFix: aBlock [ 
	"Run the block expecting an Exception. Throw an assertion failure if the block does NOT throw an exception."
	^self should: aBlock raise: Exception
]

{ #category : #asserting }
TAssertable >> shouldnt: aBlock [
	self deny: aBlock value
			
]

{ #category : #asserting }
TAssertable >> shouldnt: aBlock description: aString [
	self deny: aBlock value description: aString
			
]

{ #category : #asserting }
TAssertable >> shouldnt: aBlock raise: anExceptionalEvent [  
	 
	^self assert: (self executeShould: aBlock inScopeOf: anExceptionalEvent) not
			
]

{ #category : #asserting }
TAssertable >> shouldnt: aBlock raise: anExceptionalEvent description: aString [  
 
	^self
		assert: (self executeShould: aBlock inScopeOf: anExceptionalEvent) not
		description: aString
			
]

{ #category : #asserting }
TAssertable >> shouldnt: aBlock raise: anExceptionalEvent whoseDescriptionDoesNotInclude: substring description: aString [ 
	 
	^self
		assert: (self executeShould: aBlock inScopeOf: anExceptionalEvent withDescriptionNotContaining: substring) not
		description: aString

]

{ #category : #asserting }
TAssertable >> shouldnt: aBlock raise: anExceptionalEvent whoseDescriptionIncludes: substring description: aString [  
	 
	^self 
		assert: (self executeShould: aBlock inScopeOf: anExceptionalEvent withDescriptionContaining: substring) not
		description: aString

]

{ #category : #asserting }
TAssertable >> signalFailure: aString [
	self classForTestResult failure signal: aString
]

{ #category : #asserting }
TAssertable >> skip [
	"Don't run this test, and don't mark it as failure"
	TestSkip signal
]

{ #category : #extensions }
TAssertable >> skip: aComment [
	"Don't run this test, and don't mark it as failure"
	TestSkip signal: aComment
]

{ #category : #asserting }
TAssertable >> skipUnless: aBooleanOrBlock [
	" If the assumption in aBooleanOrBlock is not true, abandon the running test
	and mark it as passed. "
	aBooleanOrBlock value ifFalse:
			[ TestSkip signal: 'Assumption in #skipUnless: failed' ]
]

{ #category : #private }
TAssertable >> unexpectedEqualityStringBetween: actual and: expected [

	^ String streamContents: [:stream |
			stream
				nextPutAll: 'Unexpected equality of ';
				nextPutAll: actual fullPrintString;
				nextPutAll: ' and ';
				nextPutAll: expected fullPrintString;
				nextPutAll: '.']
]

{ #category : #private }
TAssertable >> unexpectedIdentityEqualityStringBetween: actual and: expected [

	^ String streamContents: [:stream |
			stream
				nextPutAll: 'Unexpected identity equality of ';
				nextPutAll: actual fullPrintString;
				nextPutAll: ' and ';
				nextPutAll: expected fullPrintString;
				nextPutAll: '.']
]
