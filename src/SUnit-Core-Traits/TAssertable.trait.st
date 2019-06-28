"
I think it is not used yet. I think it is a Trait so that we can make things assertable and not need to subclass from TestCase for example. I may expect that TestCase uses such Trait instead of implementing all those methods,  but it is not the case. Even more, TAssertable users size -> 0. 

So...seems a work half done. Adrian Kuhn introduce it. Check
http://forum.world.st/template/NamlServlet.jtp?macro=search_page&node=1294837&query=TAssertable
"
Trait {
	#name : #TAssertable,
	#category : #'SUnit-Core-Traits'
}

{ #category : #factory }
TAssertable classSide >> classForTestResult [
	"Returns the class of the test result"
	^ TestResult
]

{ #category : #factory }
TAssertable classSide >> classForTestSuite [
	^ TestSuite
]

{ #category : #logging }
TAssertable classSide >> failureLog [
	^Transcript
]

{ #category : #logging }
TAssertable classSide >> isLogging [
	"By default, we're not logging failures. Override in subclasses as desired."

	^false
]

{ #category : #logging }
TAssertable classSide >> logFailure: aString [
	self isLogging ifTrue:
		[self failureLog cr; nextPutAll: aString; flush].
]

{ #category : #asserting }
TAssertable >> assert: actualNumber closeTo: expectedNumber [
	"Tell whether the actualNumber and expectedNumber ARE close to each others with a margin of
	 the default precision (see class-side method #defaultPrecisionsForCloseToComparison).
	"
	self assert: actualNumber closeTo: expectedNumber precision: self class defaultPrecisionsForCloseToComparison
]

{ #category : #asserting }
TAssertable >> assert: actualNumber closeTo: expectedNumber precision: epsilon [
	"Tell whether the actualNumber and expectedNumber ARE close to each others with a margin of epsilon."

	^ self
		assert: (actualNumber closeTo: expectedNumber precision: epsilon)
		description: [ self comparingStringBetween: actualNumber and: expectedNumber ]
]

{ #category : #asserting }
TAssertable >> assert: aBooleanOrBlock description: aStringOrBlock [
	"This method raises an AssertionFailure with aString as #messageText if
	 aBooleanOrBlock evaluates to false.
	"
	<debuggerCompleteToSender>
	self assert: aBooleanOrBlock description: aStringOrBlock resumable: false
]

{ #category : #asserting }
TAssertable >> assert: aBooleanOrBlock description: aStringOrBlock resumable: resumableBoolean [
	"Checks if aBooleanOrBlock evaluates to true (by sending #value message to it).
	 The message text of the assertion failure exception thrown in case aBooleanOrBlock
	 evaluates to false is determinated by the evaluation of aStringOrBlock (again by
	 sending #value message to it).
	 The resumableBoolean flag is used to decide if the assertion failure signalled by
	 the source code below should be resumable or not.
	"
	<debuggerCompleteToSender>
	| exception aString |
	aBooleanOrBlock value
		ifTrue: [ ^ self ].
	
	aString := aStringOrBlock value.
	self logFailure: aString.
	exception := resumableBoolean
		ifTrue: [ self classForTestResult resumableFailure ]
		ifFalse: [ self classForTestResult failure ].
	exception signal: aString
]

{ #category : #asserting }
TAssertable >> assert: actual equals: expected [
	"This method raises an AssertionFailure if actual is different (using #= message) from expected.
	 Else it does nothing and execution continues.
	"
	
	^ self
		assert: actual = expected
		description: [self comparingStringBetween: actual and: expected]
]

{ #category : #asserting }
TAssertable >> assert: actual identicalTo: expected [
	"Verify whether the actual and the expected are the same object (have the same object pointer so #== returns true)."
	
	^ self
		assert: expected == actual
		description: [self comparingIdentityStringBetween: actual and: expected]
]

{ #category : #asserting }
TAssertable >> assertCollection: actualCollection equals: expectedCollection [
	"Raises an AssertionFailure if actualCollection is not equal to expectedCollection (using #= message).
	 I also provide a specialized message for the AssertionFailure in case I fail.
	"

	^ self
		assert: actualCollection = expectedCollection
		description: [ self comparingCollectionBetween: actualCollection and: expectedCollection ]
]

{ #category : #asserting }
TAssertable >> assertCollection: actualCollection hasSameElements: expected [
	"Assert that a collection contains the same elements as the given collection. Order is not checked, only the presence/absence of elements."

	| missingElements additionalElements |
	additionalElements := actualCollection difference: expected.
	missingElements := expected difference: (actualCollection intersection: expected).
	self
		assert: (additionalElements isEmpty and: [ missingElements isEmpty ])
		description:
			(String
				streamContents: [ :stream | 
					stream
						nextPutAll: 'Given Collections do not match!';
						lf;
						tab;
						nextPutAll: 'additions : ';
						print: additionalElements asArray;
						lf;
						tab;
						nextPutAll: 'missing: ';
						print: missingElements asArray;
						lf ])
]

{ #category : #asserting }
TAssertable >> assertCollection: actualCollection includesAll: subcollection [
	"Raises an AssertionFailure if actualCollection does not include all the elements of expectedCollection (using #includesAll: message).
	 I also provide a specialized message for the AssertionFailure in case I fail.
	"

	^ self
		assert: (actualCollection includesAll: subcollection)
		description: [ actualCollection asString , ' does not include all in ' , subcollection asString ]
]

{ #category : #asserting }
TAssertable >> assertCollection: actualCollection includesAny: subcollection [
	"Raises an AssertionFailure if actualCollection does not include any of the elements of 
	 expectedCollection (using #includesAny: message).
    I also provide a specialized message for the AssertionFailure in case I fail.
 	"
	^ self
		assert: (actualCollection includesAny: subcollection)
		description: [ actualCollection asString , ' does not include any of ' , subcollection asString ]
]

{ #category : #asserting }
TAssertable >> assertEmpty: aCollection [
	^ self assert: aCollection isEmpty description: aCollection asString , ' should have been empty'
]

{ #category : #factory }
TAssertable >> classForTestResource [
	^ TestResource
]

{ #category : #factory }
TAssertable >> classForTestResult [
	"Returns the class of the test result"
	
	"note that we did not name this method testResultClass because it is considered by tools as a test method."
	^ self class classForTestResult
]

{ #category : #factory }
TAssertable >> classForTestSuite [
	"Returns the class of the test suite"
	
	"note that we did not name this method testSuiteClass because it is considered by tools as a test method. it could be suiteClass"
	^ self class classForTestSuite
]

{ #category : #private }
TAssertable >> comparingCollectionBetween: left and: right [
	"Returns a string that shows the size if they are diferent and the aditional items for each left and right"

	| additionalLeft additionalRight sortBlock |
	"use a very slow sort block"
	sortBlock := [ :a :b | a asString <= b asString ].
	additionalLeft := (left difference: right) sorted: sortBlock.
	additionalRight := (right difference: left) sorted: sortBlock.
	^ String
		streamContents: [ :stream | 
			stream
				nextPutAll: 'Given Collections do not match. Got ';
				lf;
				tab;
				nextPutAll: 'left := ';
				print: left;
				nextPut: $.;
				lf;
				nextPutAll: ' instead of ';
				tab;
				nextPutAll: ' right :=';
				print: right;
				nextPut: $.;
				lf.
			left size = right size
				ifFalse: [ stream
						nextPutAll: 'Collection size does not match: left=';
						print: left size;
						nextPutAll: ' vs. right=';
						print: right size;
						lf ].
			additionalLeft isEmpty
				ifFalse: [ stream
						nextPutAll: 'Got ';
						print: additionalLeft size;
						nextPutAll: ' additional element(s) in the left collection: ';
						tab;
						print: additionalLeft ].
			additionalRight isEmpty
				ifFalse: [ stream
						nextPutAll: 'Got ';
						print: additionalRight size;
						nextPutAll: ' additional element(s) in the right collection: ';
						tab;
						print: additionalRight ] ]
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

{ #category : #factory }
TAssertable >> defaultTestError [

	^ self classForTestResult error
]

{ #category : #factory }
TAssertable >> defaultTestFailure [

	^ self classForTestResult failure
]

{ #category : #asserting }
TAssertable >> deny: aBooleanOrBlock [
	"This method raises an AssertionFailure if aBooleanOrBlock evaluates to true."
	self deny: aBooleanOrBlock description: 'Denial failed'
]

{ #category : #asserting }
TAssertable >> deny: actualNumber closeTo: expectedNumber [
	"Tell whether the actualNumber and expectedNumber are NOT close to each others with a margin of
	 the default precision (see class-side method #defaultPrecisionsForCloseToComparison).
	"
	self deny: actualNumber closeTo: expectedNumber precision: self class defaultPrecisionsForCloseToComparison
]

{ #category : #asserting }
TAssertable >> deny: actualNumber closeTo: expectedNumber precision: epsilon [
	"Tell whether the actualNumber and expectedNumber are NOT close to each others with a margin of epsilon."

	^ self
		assert: (actualNumber closeTo: expectedNumber precision: epsilon) not
		description: [ self comparingStringBetween: actualNumber and: expectedNumber ]
]

{ #category : #asserting }
TAssertable >> deny: aBooleanOrBlock description: aString [
	"This method raises an AssertionFailure with aString as #messageText if
	 aBooleanOrBlock evaluates to true."
	self deny: aBooleanOrBlock description: aString resumable: false
]

{ #category : #asserting }
TAssertable >> deny: aBooleanOrBlock description: aStringOrBlock resumable: resumableBoolean [
	"Checks if aBooleanOrBlock evaluates to false (by sending #value message to it).
	 aStringOrBlock and resumableBoolean parameters are used similarly as in
	 TestAsserter>>#assert:description:resumable:.
	"
	self
		assert: aBooleanOrBlock value not
		description: aStringOrBlock
		resumable: resumableBoolean
			
]

{ #category : #asserting }
TAssertable >> deny: actual equals: expected [
	"This method raises an AssertionFailure if actual is equals (using #= message) to expected.
	 Else it does nothing and execution continues.
	"
	^ self
		deny: expected = actual
		description: [self unexpectedEqualityStringBetween: actual and: expected]
]

{ #category : #asserting }
TAssertable >> deny: actual identicalTo: expected [
	"Verify whether the actual and the expected are NOT the same object (have the same object pointer so #== returns false)."
	^ self
		deny: expected == actual
		description: [self unexpectedIdentityEqualityStringBetween: actual and: expected]
]

{ #category : #asserting }
TAssertable >> denyCollection: actualCollection equals: expectedCollection [
	"Raises an AssertionFailure if actualCollection IS EQUAL to expectedCollection (using #= message).
	 I also provide a specialized message for the AssertionFailure in case I fail.
	"

	^ self
		deny: actualCollection = expectedCollection
		description: [ self unexpectedEqualityStringBetween: actualCollection and: expectedCollection ]
]

{ #category : #asserting }
TAssertable >> denyCollection: actualCollection includesAll: subcollection [
	"Raises an AssertionFailure if actualCollection INCLUDES all the elements of expectedCollection (by negating result of #includesAll: message).
	 I also provide a specialized message for the AssertionFailure in case I fail.
	"
	
	^ self
		assert: (actualCollection includesAll: subcollection) not
		description: [ actualCollection asString , ' does not include all in ' , subcollection asString ]
]

{ #category : #asserting }
TAssertable >> denyCollection: actualCollection includesAny: subcollection [
	"Raises an AssertionFailure if actualCollection includes any of the elements of 
	 expectedCollection (by negating result of #includesAny: message).
    I also provide a specialized message for the AssertionFailure in case I fail.
 	"
	^ self
		assert: (actualCollection includesAny: subcollection) not
		description: [ actualCollection asString , ' includes one element of ' , subcollection asString ]
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

{ #category : #logging }
TAssertable >> logFailure: aString [
	self class logFailure: aString.
]

{ #category : #asserting }
TAssertable >> should: aBlock [
	"Evaluate the Block first then execute the assertion"
	<debuggerCompleteToSender>
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
	<debuggerCompleteToSender>
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
	"To test that a particular exception is raised during the execution of a Block"
	<debuggerCompleteToSender>
	^ self assert: (self executeShould: aBlock inScopeOf: anExceptionalEvent)
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
	<debuggerCompleteToSender>
	^self assert: (self executeShould: aBlock inScopeOf: anException withExceptionDo: anotherBlock)
]

{ #category : #asserting }
TAssertable >> shouldFix: aBlock [
	"Run the block expecting an Exception. Throw an assertion failure if the block does NOT throw an exception."
	<debuggerCompleteToSender>
	^ self should: aBlock raise: Exception
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
	<debuggerCompleteToSender>
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
