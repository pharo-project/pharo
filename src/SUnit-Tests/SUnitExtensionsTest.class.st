"
SUnit tests for extensions to SUnit
"
Class {
	#name : #SUnitExtensionsTest,
	#superclass : #TestCase,
	#instVars : [
		'stream'
	],
	#category : #'SUnit-Tests-Core'
}

{ #category : #history }
SUnitExtensionsTest class >> lastStoredRun [
	^ ((Dictionary new) add: (#passed->((Set new) add: #testNoExceptionWithMatchingString; add: #testNoExceptionWithNoMatchingString; add: #testExceptionWithMatchingString; add: #testExceptionWithoutMatchingString; add: #testValidShouldNotTakeMoreThan; add: #testInvalidShouldNotTakeMoreThanMilliseconds; add: #testDifferentExceptionInShouldRaiseWithExceptionDo; add: #testShouldRaiseWithExceptionDo; add: #testShouldFix; add: #testAssertionFailedInRaiseWithExceptionDo; add: #testAutoDenyFalse; add: #testAutoDenyTrue; add: #testAutoAssertFalse; add: #testAutoAssertTrue; add: #testValidShouldNotTakeMoreThanMilliseconds; add: #testErrorInRaiseWithExceptionDo; add: #testNoExceptionInShouldRaiseWithExceptionDo; add: #testInvalidShouldNotTakeMoreThan; yourself)); add: (#timeStamp->'22 November 2008 10:11:35 pm'); add: (#failures->((Set new))); add: (#errors->((Set new))); yourself)
]

{ #category : #'real tests' }
SUnitExtensionsTest >> assertionFailedInRaiseWithExceptionDoTest [

	self 
		should: [ Error signal ]
		raise: Error
		withExceptionDo: [ :anException | self assert: false ]
]

{ #category : #'real tests' }
SUnitExtensionsTest >> differentExceptionInShouldRaiseWithExceptionDoTest [

	[ self 
		should: [ Error signal ]
		raise: Halt
		withExceptionDo: [ :anException | self assert: false description: 'should:raise:withExceptionDo: handled an exception that should not handle'] ]
	on: Error
	do: [ :anException | anException return: nil ]
]

{ #category : #'real tests' }
SUnitExtensionsTest >> errorInRaiseWithExceptionDoTest [

	self 
		should: [ Error  signal ]
		raise: Error
		withExceptionDo: [ :anException | Error signal: 'A forced error' ]
]

{ #category : #'test support' }
SUnitExtensionsTest >> failureLog [
	^self stream
]

{ #category : #'real tests' }
SUnitExtensionsTest >> invalidShouldNotTakeMoreThan [

	self should: [(Delay forMilliseconds: 100) wait] notTakeMoreThan: 50 milliSeconds.
]

{ #category : #'real tests' }
SUnitExtensionsTest >> invalidShouldNotTakeMoreThanMilliseconds [

	self should: [(Delay forMilliseconds: 100) wait] notTakeMoreThanMilliseconds: 50
]

{ #category : #testing }
SUnitExtensionsTest >> isLogging [
	^true
]

{ #category : #'real tests' }
SUnitExtensionsTest >> noExceptionInShouldRaiseWithExceptionDoTest [

	self 
		should: [  ]
		raise: Error
		withExceptionDo: [ :anException | Error signal: 'Should not get here' ]
]

{ #category : #'real tests' }
SUnitExtensionsTest >> shouldFixTest [

	self shouldFix: [ Error signal: 'any kind of error' ]

]

{ #category : #'real tests' }
SUnitExtensionsTest >> shouldRaiseWithExceptionDoTest [
	self should: [ Error signal: '1' ] raise: Error withExceptionDo: [ :anException | self assert: anException messageText equals: '1' ]
]

{ #category : #'real tests' }
SUnitExtensionsTest >> shouldRaiseWithSignalDoTest [
	self should: [ Error signal: '1' ] raise: Error withExceptionDo: [ :anException | self assert: anException messageText equals: '1' ]
]

{ #category : #accessing }
SUnitExtensionsTest >> stream [
	^stream ifNil: [stream := String new writeStream].
]

{ #category : #test }
SUnitExtensionsTest >> testAssertionFailedInRaiseWithExceptionDo [
	| testCase testResult |
	testCase := self class selector: #assertionFailedInRaiseWithExceptionDoTest.
	testResult := testCase run.

	self assert: (testResult failures includes: testCase).
	self assert: testResult failures size equals: 1.
	self assertEmpty: testResult passed.
	self assertEmpty: testResult errors
]

{ #category : #test }
SUnitExtensionsTest >> testAutoAssertFalse [
	| booleanCondition |
	self assert: self isLogging.
	self should: [ self assert: 1 = 2 description: 'self assert: 1 = 2' ] raise: self defaultTestFailure.
	booleanCondition := (self stream contents substrings: {Character cr}) last = 'self assert: 1 = 2'.
	self assert: booleanCondition
]

{ #category : #test }
SUnitExtensionsTest >> testAutoAssertTrue [
	self assert: 1 equals: 1.
	self assert: true
]

{ #category : #test }
SUnitExtensionsTest >> testAutoDenyFalse [
	| booleanCondition |
	self assert: self isLogging.
	self should: [ self deny: 1 = 1 description: 'self deny: 1 = 1'.] raise: self defaultTestFailure.
	booleanCondition := (self stream contents substrings:  {Character cr}) last = 'self deny: 1 = 1'.
	self assert: booleanCondition
]

{ #category : #test }
SUnitExtensionsTest >> testAutoDenyTrue [
	self deny: 1 equals: 2.
	self deny: false
]

{ #category : #test }
SUnitExtensionsTest >> testDifferentExceptionInShouldRaiseWithExceptionDo [
	| testCase testResult |
	testCase := self class selector: #differentExceptionInShouldRaiseWithExceptionDoTest.
	testResult := testCase run.

	self assert: (testResult passed includes: testCase).
	self assertEmpty: testResult errors.
	self assertEmpty: testResult failures.
	self assert: testResult passed size equals: 1
]

{ #category : #test }
SUnitExtensionsTest >> testErrorInRaiseWithExceptionDo [
	| testCase testResult |
	testCase := self class selector: #errorInRaiseWithExceptionDoTest.
	testResult := testCase run.

	self assert: (testResult errors includes: testCase).
	self assert: testResult errors size equals: 1.
	self assertEmpty: testResult failures.
	self assertEmpty: testResult passed
]

{ #category : #tests }
SUnitExtensionsTest >> testExceptionWithMatchingString [
	self should: [ Object obsolete ] raise: Error whoseDescriptionIncludes: 'NOT obsolete' description: 'tested obsoleting Object'
]

{ #category : #tests }
SUnitExtensionsTest >> testExceptionWithoutMatchingString [
	self 
		should: [ Object obsolete ]
		raise: Error
		whoseDescriptionDoesNotInclude: 'Zero'
		description: 'tested obsoleting Object'
]

{ #category : #test }
SUnitExtensionsTest >> testInvalidShouldNotTakeMoreThan [
	| testCase testResult |
	testCase := self class selector: #invalidShouldNotTakeMoreThan.
	testResult := testCase run.

	self assertEmpty: testResult passed.
	self assert: testResult failures size equals: 1.
	self assert: (testResult failures includes: testCase).
	self assertEmpty: testResult errors
]

{ #category : #test }
SUnitExtensionsTest >> testInvalidShouldNotTakeMoreThanMilliseconds [
	| testCase testResult |
	testCase := self class selector: #invalidShouldNotTakeMoreThanMilliseconds.
	testResult := testCase run.

	self assertEmpty: testResult passed.
	self assert: testResult failures size equals: 1.
	self assert: (testResult failures includes: testCase).
	self assertEmpty: testResult errors
]

{ #category : #test }
SUnitExtensionsTest >> testNoExceptionInShouldRaiseWithExceptionDo [
	| testCase testResult |
	testCase := self class selector: #noExceptionInShouldRaiseWithExceptionDoTest.
	testResult := testCase run.

	self assert: (testResult failures includes: testCase).
	self assert: testResult failures size equals: 1.
	self assertEmpty: testResult passed.
	self assertEmpty: testResult errors
]

{ #category : #tests }
SUnitExtensionsTest >> testNoExceptionWithMatchingString [
	self 
		shouldnt: [ Object messageNotUnderstoodByObject ]
		raise: MessageNotUnderstood 
		whoseDescriptionIncludes: 'Zero'
		description: 'tested obsoleting Object'
]

{ #category : #tests }
SUnitExtensionsTest >> testNoExceptionWithNoMatchingString [
	self
		shouldnt: [ Object messageNotUnderstoodByObject ]
		raise: MessageNotUnderstood
		whoseDescriptionDoesNotInclude: 'messageNotUnderstoodByObject'
		description: 'tested obsoleting Object'
]

{ #category : #test }
SUnitExtensionsTest >> testShouldFix [
	| testCase testResult |
	testCase := self class selector: #shouldFixTest.
	testResult := testCase run.

	self assert: (testResult passed includes: testCase).
	self assert: testResult passed size equals: 1.
	self assertEmpty: testResult failures.
	self assertEmpty: testResult errors
]

{ #category : #test }
SUnitExtensionsTest >> testShouldRaiseWithExceptionDo [
	| testCase testResult |
	testCase := self class selector: #shouldRaiseWithExceptionDoTest.
	testResult := testCase run.

	self assert: (testResult passed includes: testCase).
	self assert: testResult passed size equals: 1.
	self assertEmpty: testResult failures.
	self assertEmpty: testResult errors
]

{ #category : #test }
SUnitExtensionsTest >> testValidShouldNotTakeMoreThan [
	| testCase testResult |
	testCase := self class selector: #validShouldNotTakeMoreThan.
	testResult := testCase run.

	self assert: (testResult passed includes: testCase).
	self assert: testResult passed size equals: 1.
	self assertEmpty: testResult failures.
	self assertEmpty: testResult errors
]

{ #category : #test }
SUnitExtensionsTest >> testValidShouldNotTakeMoreThanMilliseconds [
	| testCase testResult |
	testCase := self class selector: #validShouldNotTakeMoreThanMilliseconds.
	testResult := testCase run.

	self assert: (testResult passed includes: testCase).
	self assert: testResult passed size equals: 1.
	self assertEmpty: testResult failures.
	self assertEmpty: testResult errors
]

{ #category : #'real tests' }
SUnitExtensionsTest >> validShouldNotTakeMoreThan [

	self should: [(Delay forMilliseconds: 100) wait] notTakeMoreThan:  200 milliSeconds.
]

{ #category : #'real tests' }
SUnitExtensionsTest >> validShouldNotTakeMoreThanMilliseconds [

	self should: [(Delay forMilliseconds: 100) wait] notTakeMoreThanMilliseconds: 200
]
