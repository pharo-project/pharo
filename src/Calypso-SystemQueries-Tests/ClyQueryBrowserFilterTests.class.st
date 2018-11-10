Class {
	#name : #ClyQueryBrowserFilterTests,
	#superclass : #ClyItemFilterTestCase,
	#category : #'Calypso-SystemQueries-Tests-Queries'
}

{ #category : #helpers }
ClyQueryBrowserFilterTests >> itemOfThisTestClass [
	^ClyBrowserItem with: self class
]

{ #category : #helpers }
ClyQueryBrowserFilterTests >> itemOfThisTestMethod [
	^ClyBrowserItem with: self class >> testSelector
]

{ #category : #'tests-filter classes' }
ClyQueryBrowserFilterTests >> testMatchesClassOfPackageWhichMatchesIt [

	filter := ClyQueryBrowserFilter substringPattern: 'SystemQueries-Tests'.
	
	self assert: (filter matches: self itemOfThisTestClass)
]

{ #category : #'tests-filter classes' }
ClyQueryBrowserFilterTests >> testMatchesClassWhichNameMatchesIt [

	filter := ClyQueryBrowserFilter substringPattern: 'FilterTests'.
	
	self assert: (filter matches: self itemOfThisTestClass)
]

{ #category : #'tests-filter methods' }
ClyQueryBrowserFilterTests >> testMatchesMethodOfClassWhichMatchesIt [

	filter := ClyQueryBrowserFilter substringPattern: 'FilterTests'.
	
	self assert: (filter matches: self itemOfThisTestMethod)
]

{ #category : #'tests-filter methods' }
ClyQueryBrowserFilterTests >> testMatchesMethodOfPackageWhichMatchesIt [

	filter := ClyQueryBrowserFilter substringPattern: 'SystemQueries-Tests'.
	
	self assert: (filter matches: self itemOfThisTestMethod)
]

{ #category : #'tests-filter methods' }
ClyQueryBrowserFilterTests >> testMatchesMethodWithSelectorWhichMatchesIt [

	filter := ClyQueryBrowserFilter substringPattern: 'MethodWithSelector'.
	
	self assert: (filter matches: self itemOfThisTestMethod)
]

{ #category : #'tests-filter classes' }
ClyQueryBrowserFilterTests >> testNotMatchesClassWithWrongNameAndPackage [

	filter := ClyQueryBrowserFilter substringPattern: 'filter not matches class name and package'.
	
	self deny: (filter matches: self itemOfThisTestClass)
]

{ #category : #'tests-filter methods' }
ClyQueryBrowserFilterTests >> testNotMatchesMethodWithWrongClassAndPackageAndSelector [

	filter := ClyQueryBrowserFilter substringPattern: 'filter not matches package, class or selector'.
	
	self deny: (filter matches: self itemOfThisTestMethod)
]

{ #category : #'tests-filter by script' }
ClyQueryBrowserFilterTests >> testUsingBadScriptWhichNotReturnBoolean [

	filter := ClyQueryBrowserFilter substringPattern: '[:each | each selector]'.
	
	self assert: (filter matches: self itemOfThisTestMethod).
	self assert: (filter matches: (ClyBrowserItem with: Object >> #printString)).
]

{ #category : #'tests-filter by script' }
ClyQueryBrowserFilterTests >> testUsingBadScriptWithoutClosingBracket [

	filter := ClyQueryBrowserFilter substringPattern: '[:each | true '.
	
	self assert: (filter matches: self itemOfThisTestMethod).
	self assert: (filter matches: (ClyBrowserItem with: Object >> #printString)).
]

{ #category : #'tests-filter by script' }
ClyQueryBrowserFilterTests >> testUsingScript [

	filter := ClyQueryBrowserFilter substringPattern: '[:each | each selector = #testUsingScript]'.
	
	self assert: (filter matches: self itemOfThisTestMethod).
	self deny: (filter matches: (ClyBrowserItem with: Object >> #printString)).
]

{ #category : #'tests-filter by script' }
ClyQueryBrowserFilterTests >> testUsingUncompiledScript [

	filter := ClyQueryBrowserFilter substringPattern: '[:each | each 123selector]'.
	
	self assert: (filter matches: self itemOfThisTestMethod).
	self assert: (filter matches: (ClyBrowserItem with: Object >> #printString)).
]
