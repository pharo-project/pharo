"
Hudson report for test coverage
"
Class {
	#name : #HDCoverageReport,
	#superclass : #HDTestReport,
	#instVars : [
		'packages',
		'wrappers',
		'covered'
	],
	#category : #'JenkinsTools-ExtraReports'
}

{ #category : #private }
HDCoverageReport >> addTestsIn: aTestAsserter to: aSet [
	(aTestAsserter isKindOf: TestSuite) ifTrue: [
		aTestAsserter tests
			do: [ :each | self addTestsIn: each to: aSet ] ].
	(aTestAsserter isKindOf: TestCase) ifTrue: [
		(aTestAsserter class respondsTo: #packageNamesUnderTest) ifTrue: [
			aTestAsserter class packageNamesUnderTest
				do: [ :each | aSet add: (RPackage organizer packageNamed: each) ] ] ].
	^ aSet
]

{ #category : #generating }
HDCoverageReport >> generate [
	| coverage |
	covered := (wrappers select: [ :each | each hasRun ])
		collect: [ :each | each reference ].
	coverage := File openForWriteFileNamed: suite name , '-Coverage.xml'.
	[ self generateOn: coverage ]
		ensure: [ coverage close ]
]

{ #category : #generating }
HDCoverageReport >> generateDataOn: aStream [
	| items |
	aStream tab; nextPutAll: '<data>'; nextPut: Character lf.
	aStream tab; tab; nextPutAll: '<all name="all classes">'; nextPut: Character lf.
	self
		generateType: 'class' indent: 3
		total: (items := (packages flatCollect: [ :each | each classes ]) asSet) size
		actual: ((covered collect: [ :each | each actualClass theNonMetaClass ]) asSet
			count: [ :each | items includes: each ])
		on: aStream.
	self
		generateType: 'method' indent: 3
		total: (items := (packages flatCollect: [ :each | each methods ]) asSet) size 
		actual: (covered count: [ :each | items includes: each ])
		on: aStream.
	packages do: [ :each | self generatePackage: each on: aStream ].
	aStream tab; tab; nextPutAll: '</all>'; nextPut: Character lf.
	aStream tab; nextPutAll: '</data>'; nextPut: Character lf
]

{ #category : #generating }
HDCoverageReport >> generateOn: aStream [
	aStream nextPutAll: '<?xml version="1.0" encoding="UTF-8"?>'; nextPut: Character lf.
	aStream nextPutAll: '<report>'; nextPut: Character lf.
	self generateStatsOn: aStream.
	self generateDataOn: aStream.
	aStream nextPutAll: '</report>'; nextPut: Character lf
]

{ #category : #generating }
HDCoverageReport >> generatePackage: aPackage class: aClass on: aStream [
	| items |
	aStream tab: 4; nextPutAll: '<class name="'; nextPutAll: (self encode: aClass name); nextPutAll: '">'; nextPut: Character lf.
	self
		generateType: 'class' indent: 5
		total: 1
		actual: ((covered anySatisfy: [ :each | each actualClass theNonMetaClass = aClass ])
			ifTrue: [ 1 ] ifFalse: [ 0 ])
		on: aStream.
	self
		generateType: 'method' indent: 5
		total: (items := aPackage definedMethodsForClass: aClass) size
		actual: (covered count: [ :each | items includes: each ])
		on: aStream.
	items do: [ :each | self generatePackage: each method: each on: aStream ].	
	aStream tab: 4; nextPutAll: '</class>'; nextPut: Character lf
]

{ #category : #generating }
HDCoverageReport >> generatePackage: aPackage method: aReference on: aStream [
 
	aStream tab: 5; nextPutAll: '<method name="'; nextPutAll: (self encode: aReference selector); nextPutAll: '">'; nextPut: Character lf.
	self
		generateType: 'method' indent: 6
		total: 1
		actual: ((covered includes: aReference) ifTrue: [ 1 ] ifFalse: [ 0 ])
		on: aStream.
	aStream tab: 5; nextPutAll: '</method>'; nextPut: Character lf
]

{ #category : #generating }
HDCoverageReport >> generatePackage: aPackage on: aStream [
	| items |
	aStream tab: 3; nextPutAll: '<package name="'; nextPutAll: (self encode: (aPackage packageName copyReplaceAll: '-' with: '.')); nextPutAll: '">'; nextPut: Character lf.
	self
		generateType: 'class' indent: 4
		total: (items := aPackage classes asSet) size
		actual: ((covered collect: [ :each | each actualClass theNonMetaClass ]) asSet
			count: [ :each | items includes: each ])
		on: aStream.
	self
		generateType: 'method' indent: 4
		total: (items := aPackage methods asSet) size
		actual: (covered count: [ :each | items includes: each ])
		on: aStream.
	aPackage classes 
		do: [ :class | self generatePackage: aPackage class: class on: aStream ].
	aStream tab: 3; nextPutAll: '</package>'; nextPut: Character lf
]

{ #category : #generating }
HDCoverageReport >> generateStatsOn: aStream [
	aStream tab; nextPutAll: '<stats>'; nextPut: Character lf.
	aStream tab; tab; nextPutAll: '<packages value="'; print: (packages size); nextPutAll: '"/>'; nextPut: Character lf.
	aStream tab; tab; nextPutAll: '<classes value="'; print: (packages detectSum: [ :each | each classes size ]); nextPutAll: '"/>'; nextPut: Character lf.
	aStream tab; tab; nextPutAll: '<methods value="'; print: (packages detectSum: [ :each | each methods size ]); nextPutAll: '"/>'; nextPut: Character lf.
	aStream tab; nextPutAll: '</stats>'; nextPut: Character lf.
]

{ #category : #generating }
HDCoverageReport >> generateType: aString indent: anInteger total: totalInteger actual: actualInteger on: aStream [
	aStream tab: anInteger; 
		nextPutAll: '<coverage type="'; nextPutAll: aString; 
		nextPutAll: ', %" value="'; print: (totalInteger = 0 ifTrue: [ 0 ] ifFalse: [ (100.0 * actualInteger / totalInteger) rounded ]); 
		nextPutAll: '% ('; print: actualInteger; nextPut: $/; print: totalInteger; nextPutAll: ')"/>'; 
		nextPut: Character lf
]

{ #category : #private }
HDCoverageReport >> ignoredSelectors [
	^ #(packageNamesUnderTest classNamesNotUnderTest)
]

{ #category : #private }
HDCoverageReport >> methodsIn: aPackage [
	aPackage ifNil: [ ^ #() ].
	^ aPackage methods reject: [ :method | 
		(self ignoredSelectors includes: method selector)
			or: [ method compiledMethod isAbstract
			or: [ method compiledMethod refersToLiteral: #ignoreForCoverage ] ] ]
]

{ #category : #private }
HDCoverageReport >> packagesIn: aTestAsserter [
	^ self addTestsIn: aTestAsserter to: Set new
]

{ #category : #running }
HDCoverageReport >> setUp [
	super setUp.
	wrappers := ((packages := self packagesIn: suite)
		flatCollect: [ :package | self methodsIn: package ])
		collect: [ :each | HDTestCoverage on: each ].
	wrappers do: [ :each | each install ]
]

{ #category : #running }
HDCoverageReport >> tearDown [
	wrappers do: [ :each | each uninstall ].
	super tearDown.
	self generate
]
