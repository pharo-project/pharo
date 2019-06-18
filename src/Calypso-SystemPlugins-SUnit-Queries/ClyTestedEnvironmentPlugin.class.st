"
I mark covered methods with information about test which cover them.

I decorate covered methods with ClyTestedMethodProperty which includes information about test result (ClyTestResultProperty). 

To find covering test I use simple hueristics: 
- Test or Tests suffix 
- test prefix for method name with various combinations.
Look at ""tests lookup"" methods for details.
"
Class {
	#name : #ClyTestedEnvironmentPlugin,
	#superclass : #ClySystemEnvironmentPlugin,
	#category : #'Calypso-SystemPlugins-SUnit-Queries'
}

{ #category : #testing }
ClyTestedEnvironmentPlugin class >> isSlow [
	^true
]

{ #category : #controlling }
ClyTestedEnvironmentPlugin >> attachToSystem [

	TestCase historyAnnouncer 
		weak when: TestSuiteEnded send: #testRan: to: self
]

{ #category : #'item decoration' }
ClyTestedEnvironmentPlugin >> decorateBrowserItem: anItem ofClass: aClass [

	| testCase |
	aClass isTestCase ifTrue: [ ^self ].

	testCase := self findTestCaseCovering: aClass ifAbsent: [^self].
	
	anItem addProperty: (ClyTestedClassProperty coveringTestCase: testCase)
]

{ #category : #'item decoration' }
ClyTestedEnvironmentPlugin >> decorateBrowserItem: anItem ofMethod: aMethod [

	| testMethod testResult |
	aMethod isTestMethod ifTrue: [ ^self ].

	testMethod := self findTestMethodCovering: aMethod ifAbsent: [^self].
	
	testResult := (ClyTestResultProperty ofMethod: testMethod).
	anItem addProperty: (ClyTestedMethodProperty coveringTest: testMethod result: testResult)
]

{ #category : #controlling }
ClyTestedEnvironmentPlugin >> detachFromSystem [

	TestCase historyAnnouncer unsubscribe: self
]

{ #category : #'tests lookup' }
ClyTestedEnvironmentPlugin >> findTestCaseCovering: aClass ifAbsent: absentBlock [

	| className |
	className := aClass instanceSide name.
	
	^environment system classNamed: (className , 'Test') asSymbol ifAbsent: [ 
		environment system classNamed: (className , 'Tests') asSymbol ifAbsent: absentBlock ]
]

{ #category : #'tests lookup' }
ClyTestedEnvironmentPlugin >> findTestMethodCovering: aMethod ifAbsent: aBlock [
	| selectorString testCase |
	aMethod isTestMethod ifTrue: [ ^ aBlock value ].
	testCase := self findTestCaseCovering: aMethod methodClass ifAbsent: [^aBlock value].
	
	selectorString := 'test', (self normalizeSelectorForComparison: aMethod selector).
	
	^ testCase methods
		detect: [ :each | (self normalizeSelectorForComparison: each selector) = selectorString ]
		ifNone: aBlock
]

{ #category : #'tests lookup' }
ClyTestedEnvironmentPlugin >> findTestedClassCoveredBy: aTestClass ifAbsent: absentBlock [

	| className |
	className := aTestClass instanceSide name.
	
	(className endsWith: 'Tests') ifTrue: [ 
		^environment system 
			classNamed: (className allButLast: 5) asSymbol ifAbsent: absentBlock ].
	(className endsWith: 'Test') ifTrue: [ 
		^environment system 
			classNamed: (className allButLast: 4) asSymbol  ifAbsent: absentBlock ].
	
	^absentBlock value
]

{ #category : #'tests lookup' }
ClyTestedEnvironmentPlugin >> normalizeSelectorForComparison: aString [
	"Returns a normalized version of given string without colons and in all lowercase"
	| selectorString selectorStringSize normalizedSelectorString normalizedSelectorStringSize |
	"asString to convert Symbols"
	selectorString := aString asString.
	selectorStringSize := aString size.
	normalizedSelectorString := selectorString class new: selectorStringSize.
	normalizedSelectorStringSize := 0.
	"this inlined to:do: loop with direct string manipulation is faster than
	using copyWithout:, which relies on reject:"
	1 to: selectorStringSize do: [ :i | 
		| nextChar |
		(nextChar := selectorString at: i) == $:
			ifFalse: [
				normalizedSelectorString
					at: (normalizedSelectorStringSize := normalizedSelectorStringSize + 1)
					put: nextChar ] ].
	"faster to use asLowercase on the entire string after instead of on chars
	in the to:do: loop because of the bad Character>>#asLowercase implementation"
	^ (normalizedSelectorStringSize < selectorStringSize
		ifTrue: [ normalizedSelectorString first: normalizedSelectorStringSize ]
		ifFalse: [ selectorString ]) asLowercase
]

{ #category : #controlling }
ClyTestedEnvironmentPlugin >> testRan: aTestSuiteEnded [
	| event runTestCase testedClass |
	
	"testResult actually returns the TestCase class ran"	
	runTestCase := aTestSuiteEnded testResult.
	testedClass := self findTestedClassCoveredBy: runTestCase ifAbsent: [ ^self ].
	"Here we just reuse normal event with tested class which is covered by runTestCase.
	Logic for updates is same in that case"	
	event := ClyTestCaseRan testCase: testedClass.
	environment systemChanged: event
]
