"
This implements the Collecting Parameter pattern for running a bunch of tests.  It holds tests that have run, sorted into the result categories of passed, failures and errors.

TestResult is an interesting object to subclass or substitute. #runCase: is the external protocol you need to reproduce. TestResult subclasses can  handle multi-threaded tests (see SUnitXProcPatterns) and might record coverage information or send emails when the run completes.

"
Class {
	#name : #TestResult,
	#superclass : #Object,
	#instVars : [
		'timeStamp',
		'failures',
		'errors',
		'passed',
		'skipped'
	],
	#category : #'SUnit-Core-Kernel'
}

{ #category : #exceptions }
TestResult class >> error [
	^self exError
]

{ #category : #exceptions }
TestResult class >> exError [
	^Error
			
]

{ #category : #exceptions }
TestResult class >> failure [
	^TestFailure
]

{ #category : #history }
TestResult class >> historyAt: aTestCaseClass [
"I will return the last test dictionary for aTestCaseClass. If none found, I will create a new empty one and link it in the history."

	^ aTestCaseClass history 
]

{ #category : #history }
TestResult class >> historyAt: aTestCaseClass put: aDictionary [
	aTestCaseClass history: aDictionary
	"^ self history at: aTestCaseClass put: aDictionary "
]

{ #category : #history }
TestResult class >> historyFor: aTestCaseClass [
	"I return the last test dictionary for aTestCaseClass. 
	If none found, I return an empty dictionary but will not link it to the class in the history."
	
	| history |
	history := aTestCaseClass history.
	history ifNil: [ ^ self newTestDictionary ].
	^ history
	
"	^ self history at: aTestCaseClass ifAbsent: [ self newTestDictionary ]"
]

{ #category : #history }
TestResult class >> newTestDictionary [

	^ Dictionary new at: #timeStamp put: DateAndTime now;
		at: #passed put: Set new;
		at: #failures put: Set new;
		at: #errors put: Set new;
		yourself
		
]

{ #category : #history }
TestResult class >> removeFromTestHistory: aSelector in: aTestCaseClass [
	| lastRun |
	
	lastRun := self historyFor: aTestCaseClass.
	#(#passed #failures #errors) do:
		[ :set | (lastRun at: set) remove: aSelector ifAbsent: []].

]

{ #category : #exceptions }
TestResult class >> resumableFailure [
	^ResumableTestFailure
]

{ #category : #exceptions }
TestResult class >> signalErrorWith: aString [ 
	self error signal: aString
			
]

{ #category : #exceptions }
TestResult class >> signalFailureWith: aString [ 
	self failure signal: aString
			
]

{ #category : #exceptions }
TestResult class >> skip [
	^ TestSkip
]

{ #category : #history }
TestResult class >> updateTestHistoryFor: aTestCase status: aSymbol [
	| cls sel |
	
	cls := aTestCase class.
	sel := aTestCase selector.
	self removeFromTestHistory: sel in: cls.
	((self historyAt: cls) at: aSymbol ) add: sel.
]

{ #category : #exceptions }
TestResult class >> warning [
	"Warning that should be treated as test failure"
	^Deprecation 
			
]

{ #category : #adding }
TestResult >> addError: aTestCase [
	"We cannot use self errors as that incorporates test expectations and so does not return the stored collection."

	^errors add: aTestCase
]

{ #category : #adding }
TestResult >> addFailure: aTestCase [
	"We cannot use self failures as that incorporates test expectations and so does not return the stored collection."

	^failures add: aTestCase
]

{ #category : #adding }
TestResult >> addPass: aTestCase [
	"We cannot use self passed as that incorporates test expectations and so does not return the stored collection."

	^passed add: aTestCase
]

{ #category : #adding }
TestResult >> addSkip: aTestCase [

	^skipped add: aTestCase
]

{ #category : #accessing }
TestResult >> classesTested [
	^ (self tests collect: [ :testCase | testCase class ]) asSet
]

{ #category : #accessing }
TestResult >> defects [
	^OrderedCollection new
		addAll: self errors;
		addAll: self failures; yourself
]

{ #category : #diff }
TestResult >> diff: aTestResult [
	"Return a collection that contains differences"
	| passed1Selectors failed1Selectors errors1Selectors passed2Selectors failed2Selectors errors2Selectors |
	passed1Selectors := self passed collect: [:testCase | testCase selector].
	failed1Selectors := self failures collect: [:testCase | testCase selector].
	errors1Selectors := self errors collect: [:testCase | testCase selector].

	passed2Selectors := aTestResult passed collect: [:testCase | testCase selector].
	failed2Selectors := aTestResult failures collect: [:testCase | testCase selector].
	errors2Selectors := aTestResult errors collect: [:testCase | testCase selector].
	
	^ {passed1Selectors copyWithoutAll: passed2Selectors .
		failed1Selectors copyWithoutAll: failed2Selectors .
		errors1Selectors copyWithoutAll: errors2Selectors}
]

{ #category : #history }
TestResult >> dispatchResultsIntoHistory [ 

	self classesTested do: 
		[ :testClass | 
		self class
			historyAt: testClass
			put: (self selectResultsForTestCase: testClass) ].

]

{ #category : #accessing }
TestResult >> errorCount [

	^self errors size
]

{ #category : #compatibility }
TestResult >> errors [
	^ self unexpectedErrors
]

{ #category : #accessing }
TestResult >> errors: anOrderedCollection [
	errors := anOrderedCollection
]

{ #category : #accessing }
TestResult >> expectedDefectCount [
	^ self expectedDefects size
]

{ #category : #accessing }
TestResult >> expectedDefects [
	^ (errors, failures asOrderedCollection) select: [:each | each shouldPass not] 
]

{ #category : #accessing }
TestResult >> expectedPassCount [
	^ self expectedPasses size
]

{ #category : #accessing }
TestResult >> expectedPasses [
	^ passed select: [:each | each shouldPass] 
]

{ #category : #accessing }
TestResult >> failureCount [

	^self failures size
]

{ #category : #compatibility }
TestResult >> failures [
	^ self unexpectedFailures, self unexpectedPasses 
]

{ #category : #accessing }
TestResult >> failures: aSet [
	failures := aSet
]

{ #category : #'file in/out' }
TestResult >> fileOutOn: aFileStream [
	"Write in aFileStream like:
	3 run, 2 passes, 0 expected failures, 1 failures, 0 errors, 0 unexpected passes
	Failures:
	SHParserST80Test>>#testNumbers
	
	Errors:"	
	| printer |
	printer := [:title :testCases | 
				aFileStream cr; nextPutAll: title; cr.
				testCases do: [:aTestCase| 
								aTestCase printOn: aFileStream.
								aFileStream cr]].
				
	self printOn: aFileStream.	
	printer value: 'Failures:' value: self failures.
	printer value: 'Errors:' value: self errors.
]

{ #category : #testing }
TestResult >> hasErrors [

	^self errors size > 0
]

{ #category : #testing }
TestResult >> hasFailures [

	^self failures size > 0
]

{ #category : #testing }
TestResult >> hasPassed [

	^self hasErrors not and: [self hasFailures not]
]

{ #category : #initialization }
TestResult >> initialize [
	super initialize.
	passed := OrderedCollection new.
	failures := Set new.
	errors := OrderedCollection new.
	skipped := OrderedCollection new.
	timeStamp := DateAndTime now
]

{ #category : #testing }
TestResult >> isError: aTestCase [

	^self errors includes: aTestCase
]

{ #category : #querying }
TestResult >> isErrorFor: class selector: selector [
	^ self errors anySatisfy: [:testCase | testCase class == class and: [testCase selector == selector]]
]

{ #category : #testing }
TestResult >> isFailure: aTestCase [
	^self failures includes: aTestCase
]

{ #category : #querying }
TestResult >> isFailureFor: class selector: selector [
	^ self failures anySatisfy: [:testCase | testCase class == class and: [testCase selector == selector]]
]

{ #category : #testing }
TestResult >> isPassed: aTestCase [

	^self passed includes: aTestCase
]

{ #category : #querying }
TestResult >> isPassedFor: class selector: selector [
	^ self passed anySatisfy: [:testCase | testCase class == class and: [testCase selector == selector]]
]

{ #category : #compatibility }
TestResult >> passed [
	^ self expectedPasses, self expectedDefects
]

{ #category : #accessing }
TestResult >> passed: anOrderedCollection [
	passed := anOrderedCollection
]

{ #category : #accessing }
TestResult >> passedCount [

	^self passed size
]

{ #category : #printing }
TestResult >> printOn: aStream [
	self runCount isZero ifTrue: [ ^ aStream nextPutAll: 'No tests ran' ].
	aStream
		print: self runCount;
		nextPutAll: ' ran, ';
		print: self expectedPassCount;
		nextPutAll: ' passed, ';
		print: self skippedCount;
		nextPutAll: ' skipped, ';
		print: self expectedDefectCount;
		nextPutAll: (' expected failure' asPluralBasedOn: self expectedDefectCount); 
		nextPutAll: ', ';
		print: self unexpectedFailureCount;
		nextPutAll: (' failure' asPluralBasedOn: self unexpectedFailureCount);
		nextPutAll: ', ';
		print: self unexpectedErrorCount;
		nextPutAll: (' error' asPluralBasedOn: self unexpectedErrorCount);
		nextPutAll: ', ';
		print: self unexpectedPassCount;
		nextPutAll: ' passed unexpected'
]

{ #category : #running }
TestResult >> runCase: aTestCase [
	[
	aTestCase announce: TestCaseStarted withResult: self.
	aTestCase runCaseManaged.
	aTestCase announce: TestCaseEnded  withResult: self.
	self addPass: aTestCase]
		on: self class failure , self class skip, self class warning, self class error
		do: [:ex | ex sunitAnnounce: aTestCase toResult: self]
]

{ #category : #running }
TestResult >> runCaseForDebug: aTestCase [
	[
	aTestCase announce: TestCaseStarted withResult: self.
	aTestCase runCaseManaged.
	aTestCase announce: TestCaseEnded  withResult: self.
	"To not affect performance of big test suites following logic is not inside addPass: method"
	errors remove: aTestCase ifAbsent: [].
	failures remove: aTestCase ifAbsent: [].
	self addPass: aTestCase]
		on: self class failure , self class skip, self class warning, self class error
		do: [:ex | ex sunitAnnounce: aTestCase toResult: self. ex pass]
]

{ #category : #accessing }
TestResult >> runCount [

	^self passedCount + self failureCount + self errorCount
]

{ #category : #history }
TestResult >> selectResultsForTestCase: aTestCaseClass [
	| passedSelectors errorsSelectors failuresSelectors |
	passedSelectors := self passed
						select: [:testCase | testCase class == aTestCaseClass ] thenCollect: [:testCase | testCase selector].
	errorsSelectors := self errors 
						select: [:testCase | testCase class == aTestCaseClass ] thenCollect:  [:testCase | testCase selector].
	failuresSelectors := self failures 
						select: [:testCase | testCase class == aTestCaseClass ] thenCollect:  [:testCase | testCase selector].
	
	^ self class newTestDictionary 
		at: #passed put: passedSelectors asSet;
		at: #failures put: failuresSelectors asSet;
		at: #errors put: errorsSelectors asSet;
		yourself
		
]

{ #category : #accessing }
TestResult >> skipped [
	^ skipped
]

{ #category : #accessing }
TestResult >> skippedCount [

	^ self skipped size
]

{ #category : #accessing }
TestResult >> tests [
	^(OrderedCollection new: self runCount)
		addAll: passed;
		addAll: failures;
		addAll: errors;
		addAll: skipped;
		yourself
]

{ #category : #accessing }
TestResult >> timeStamp [
	^ timeStamp
]

{ #category : #accessing }
TestResult >> timeStamp: anObject [
	timeStamp := anObject
]

{ #category : #accessing }
TestResult >> unexpectedErrorCount [
	^ self unexpectedErrors size
]

{ #category : #accessing }
TestResult >> unexpectedErrors [
	^ errors select: [:each | each shouldPass] 
]

{ #category : #accessing }
TestResult >> unexpectedFailureCount [
	^ self unexpectedFailures size
]

{ #category : #accessing }
TestResult >> unexpectedFailures [
	^ failures select: [:each | each shouldPass] 
]

{ #category : #accessing }
TestResult >> unexpectedPassCount [
	^ self unexpectedPasses size
]

{ #category : #accessing }
TestResult >> unexpectedPasses [
	^ passed select: [:each | each shouldPass not] 
]

{ #category : #history }
TestResult >> updateResultsInHistory [
	|classesToNotify|
	classesToNotify:= Set new.
	#(#passed #failures #errors) do: [ :status | 
		(self perform: status) do: [ :testCase | 
			classesToNotify add:testCase class.
			self class updateTestHistoryFor: testCase status: status ] ].
	classesToNotify do:[:cl | 
		cl historyAnnouncer announce: (TestSuiteEnded result: cl)]
]
