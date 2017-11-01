"
I am a helper object used for validating formatted Date strings.


"
Class {
	#name : #DatePrintFormatTester,
	#superclass : #Object,
	#instVars : [
		'dayPosition',
		'monthPosition',
		'yearPosition',
		'delimiter',
		'monthType',
		'yearType',
		'date'
	],
	#category : 'Kernel-Tests-Chronology'
}

{ #category : #'instance creation' }
DatePrintFormatTester class >> on: aDate [

	^ self new date: aDate.
]

{ #category : #asserting }
DatePrintFormatTester >> assert: aBoolean [
	"We check for true explicitly to make the assertion fail for non booleans"
	aBoolean == true ifFalse: [self signalFailure: 'Assertion failed']
			
]

{ #category : #asserting }
DatePrintFormatTester >> assert: actualNumber closeTo: expectedNumber [
	^ self
		assert: (actualNumber closeTo: expectedNumber)
		description: [self comparingStringBetween: actualNumber and: expectedNumber]
]

{ #category : #asserting }
DatePrintFormatTester >> assert: aBooleanOrBlock description: aStringOrBlock [
	aBooleanOrBlock value ifFalse: [
		| message |
		message := aStringOrBlock value.
		self classForTestResult failure signal: message]
			
]

{ #category : #asserting }
DatePrintFormatTester >> assert: aBooleanOrBlock description: aStringOrBlock resumable: resumableBoolean [ 
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
DatePrintFormatTester >> assert: actual equals: expected [
	^ self
		assert: expected = actual
		description: [self comparingStringBetween: actual and: expected]
]

{ #category : #asserting }
DatePrintFormatTester >> assert: actual identicalTo: expected [
	^ self
		assert: expected == actual
		description: [self comparingIdentityStringBetween: actual and: expected]
]

{ #category : #asserting }
DatePrintFormatTester >> assertCollection: actual equals: expected [
	"Specialized test method that generates a proper error message for collection"
	^ self
		assert: expected = actual
		description: [ self comparingCollectionBetween: actual and: expected ]
]

{ #category : #asserting }
DatePrintFormatTester >> assertCollection: actual hasSameElements: expected [
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
DatePrintFormatTester >> classForTestResult [
	"Returns the class of the test result"
	^ TestResult
]

{ #category : #private }
DatePrintFormatTester >> comparingCollectionBetween: left and: right [
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
DatePrintFormatTester >> comparingIdentityStringBetween: actual and: expected [

	^ String streamContents: [:stream |
			stream
				nextPutAll: actual fullPrintString;
				nextPutAll: ' is not identical to ';
				nextPutAll: expected fullPrintString;
				nextPutAll: '.']
]

{ #category : #private }
DatePrintFormatTester >> comparingStringBetween: actual and: expected [

	^ String streamContents: [:stream |
			stream
				nextPutAll: 'Got ';
				nextPutAll: actual fullPrintString;
				nextPutAll: ' instead of ';
				nextPutAll: expected fullPrintString;
				nextPutAll: '.']
]

{ #category : #accessing }
DatePrintFormatTester >> date: aDate [ 

	date := aDate.
]

{ #category : #private }
DatePrintFormatTester >> dayPosition: dayPos monthPosition: moPos yearPosition: yrPos delimiter: aCharacter monthType: moType yearType: yrType [ 

	dayPosition := dayPos.
	monthPosition := moPos.
	yearPosition := yrPos.
	delimiter := aCharacter.
	monthType := moType.
	yearType := yrType.
	^ self.
]

{ #category : #accessing }
DatePrintFormatTester >> delimiter: aCharacter [ 

	delimiter := aCharacter.
]

{ #category : #asserting }
DatePrintFormatTester >> deny: aBooleanOrBlock [

	self assert: aBooleanOrBlock value not
			
]

{ #category : #asserting }
DatePrintFormatTester >> deny: aBooleanOrBlock description: aString [
	self assert: aBooleanOrBlock value not description: aString
			
]

{ #category : #asserting }
DatePrintFormatTester >> deny: aBooleanOrBlock description: aString resumable: resumableBoolean [ 
	self
		assert: aBooleanOrBlock value not
		description: aString
		resumable: resumableBoolean
			
]

{ #category : #asserting }
DatePrintFormatTester >> deny: actual equals: expected [
	^ self
		deny: expected = actual
		description: [self unexpectedEqualityStringBetween: actual and: expected]
]

{ #category : #asserting }
DatePrintFormatTester >> deny: actual identicalTo: expected [
	^ self
		deny: expected == actual
		description: [self unexpectedIdentityEqualityStringBetween: actual and: expected]
]

{ #category : #asserting }
DatePrintFormatTester >> denyCollection: actual equals: expected [
	"Specialized test method that generates a proper error message for collection"
	^ self
		deny: expected = actual
		description: [ self unexpectedEqualityStringBetween: actual and: expected ]
]

{ #category : #asserting }
DatePrintFormatTester >> executeShould: aBlock inScopeOf: anExceptionalEvent [ 
	^[aBlock value.
 	false] on: anExceptionalEvent
		do: [:ex | ex return: true]
			
]

{ #category : #asserting }
DatePrintFormatTester >> executeShould: aBlock inScopeOf: anExceptionalEvent withDescriptionContaining: aString [
	^[aBlock value.
 	false] on: anExceptionalEvent
		do: [:ex | ex return: (ex description includesSubstring: aString) ]
			
]

{ #category : #asserting }
DatePrintFormatTester >> executeShould: aBlock inScopeOf: anExceptionalEvent withDescriptionNotContaining: aString [
	^[aBlock value.
 	false] on: anExceptionalEvent
		do: [:ex | ex return: (ex description includesSubstring: aString) not ]
			
]

{ #category : #asserting }
DatePrintFormatTester >> executeShould: aBlock inScopeOf: anException withExceptionDo: anotherBlock [

	^[aBlock value.
 	false] 
		on: anException
		do: [:exception | 
			anotherBlock value: exception.
			exception return: true]
]

{ #category : #asserting }
DatePrintFormatTester >> fail [

	^self assert: false
]

{ #category : #asserting }
DatePrintFormatTester >> fail: aDescriptionString [

	^self assert: false description: aDescriptionString
]

{ #category : #accessing }
DatePrintFormatTester >> monthType: anInteger [ 

	monthType := anInteger.
]

{ #category : #asserting }
DatePrintFormatTester >> monthType: aNumber shouldPrintAs: aString [ 

	| actual |
	monthType := aNumber.
	actual := self tokens at: monthPosition.
	self assert: actual equals: aString.
]

{ #category : #printing }
DatePrintFormatTester >> printFormat [

	^ date printFormat: { dayPosition. monthPosition. yearPosition. delimiter. monthType. yearType }.
]

{ #category : #asserting }
DatePrintFormatTester >> should: aBlock [
	self assert: aBlock value
			
]

{ #category : #asserting }
DatePrintFormatTester >> should: aBlock description: aString [
	self assert: aBlock value description: aString
			
]

{ #category : #asserting }
DatePrintFormatTester >> should: aBlock notTakeMoreThan: aDuration [
    "Evaluate aBlock and if it takes more than given duration
    to run we report a test failure. "

	^ aBlock valueWithin: aDuration onTimeout: [
		self 
			assert: false 
			description: ['Block evaluation took more than the expected <1p>' expandMacrosWith: aDuration]
	]
	

]

{ #category : #asserting }
DatePrintFormatTester >> should: aBlock notTakeMoreThanMilliseconds: anInteger [
    "For compatibility with other Smalltalks"

   self should: aBlock notTakeMoreThan: (Duration milliSeconds: anInteger).
]

{ #category : #asserting }
DatePrintFormatTester >> should: aBlock raise: anExceptionalEvent [ 
	^self assert: (self executeShould: aBlock inScopeOf: anExceptionalEvent)
			
]

{ #category : #asserting }
DatePrintFormatTester >> should: aBlock raise: anExceptionalEvent description: aString [ 
	^self assert: (self executeShould: aBlock inScopeOf: anExceptionalEvent)
		description: aString
			
]

{ #category : #asserting }
DatePrintFormatTester >> should: aBlock raise: anExceptionalEvent whoseDescriptionDoesNotInclude: substring description: aString [ 
	^self assert: (self executeShould: aBlock inScopeOf: anExceptionalEvent withDescriptionNotContaining: substring)
		description: aString

]

{ #category : #asserting }
DatePrintFormatTester >> should: aBlock raise: anExceptionalEvent whoseDescriptionIncludes: substring description: aString [ 
	^self assert: (self executeShould: aBlock inScopeOf: anExceptionalEvent withDescriptionContaining: substring)
		description: aString

]

{ #category : #asserting }
DatePrintFormatTester >> should: aBlock raise: anException withExceptionDo: anotherBlock [ 

	^self assert: (self executeShould: aBlock inScopeOf: anException withExceptionDo: anotherBlock)
]

{ #category : #asserting }
DatePrintFormatTester >> shouldEqual: expectedOutputString [ 

	self assert: self printFormat equals: expectedOutputString.

]

{ #category : #asserting }
DatePrintFormatTester >> shouldFix: aBlock [ 
	"Run the block expecting an Exception. Throw an assertion failure if the block does NOT throw an exception."
	^self should: aBlock raise: Exception
]

{ #category : #asserting }
DatePrintFormatTester >> shouldnt: aBlock [
	self deny: aBlock value
			
]

{ #category : #asserting }
DatePrintFormatTester >> shouldnt: aBlock description: aString [
	self deny: aBlock value description: aString
			
]

{ #category : #asserting }
DatePrintFormatTester >> shouldnt: aBlock raise: anExceptionalEvent [  
	 
	^self assert: (self executeShould: aBlock inScopeOf: anExceptionalEvent) not
			
]

{ #category : #asserting }
DatePrintFormatTester >> shouldnt: aBlock raise: anExceptionalEvent description: aString [  
 
	^self
		assert: (self executeShould: aBlock inScopeOf: anExceptionalEvent) not
		description: aString
			
]

{ #category : #asserting }
DatePrintFormatTester >> shouldnt: aBlock raise: anExceptionalEvent whoseDescriptionDoesNotInclude: substring description: aString [ 
	 
	^self
		assert: (self executeShould: aBlock inScopeOf: anExceptionalEvent withDescriptionNotContaining: substring) not
		description: aString

]

{ #category : #asserting }
DatePrintFormatTester >> shouldnt: aBlock raise: anExceptionalEvent whoseDescriptionIncludes: substring description: aString [  
	 
	^self 
		assert: (self executeShould: aBlock inScopeOf: anExceptionalEvent withDescriptionContaining: substring) not
		description: aString

]

{ #category : #asserting }
DatePrintFormatTester >> signalFailure: aString [
	self classForTestResult failure signal: aString
]

{ #category : #asserting }
DatePrintFormatTester >> skip [
	"Don't run this test, and don't mark it as failure"
	TestSkip signal
]

{ #category : #extensions }
DatePrintFormatTester >> skip: aComment [
	"Don't run this test, and don't mark it as failure"
	TestSkip signal: aComment
]

{ #category : #asserting }
DatePrintFormatTester >> skipUnless: aBooleanOrBlock [
	" If the assumption in aBooleanOrBlock is not true, abandon the running test
	and mark it as passed. "
	aBooleanOrBlock value ifFalse:
			[ TestSkip signal: 'Assumption in #skipUnless: failed' ]
]

{ #category : #asserting }
DatePrintFormatTester >> tokens [

	^ self printFormat findTokens: { delimiter }.
]

{ #category : #private }
DatePrintFormatTester >> unexpectedEqualityStringBetween: actual and: expected [

	^ String streamContents: [:stream |
			stream
				nextPutAll: 'Unexpected equality of ';
				nextPutAll: actual fullPrintString;
				nextPutAll: ' and ';
				nextPutAll: expected fullPrintString;
				nextPutAll: '.']
]

{ #category : #private }
DatePrintFormatTester >> unexpectedIdentityEqualityStringBetween: actual and: expected [

	^ String streamContents: [:stream |
			stream
				nextPutAll: 'Unexpected identity equality of ';
				nextPutAll: actual fullPrintString;
				nextPutAll: ' and ';
				nextPutAll: expected fullPrintString;
				nextPutAll: '.']
]

{ #category : #asserting }
DatePrintFormatTester >> yearType: aNumber shouldPrintAs: aString [ 

	| actual tokens isMonthMissing yearTokenIndex |
	yearType := aNumber.
	tokens := self tokens.
	isMonthMissing := tokens size = 2.
	yearTokenIndex := isMonthMissing
		ifTrue: [ 2 ]
		ifFalse: [ 3 ].
	actual := tokens at: yearTokenIndex.
	
	self assert: actual equals: aString.
]
