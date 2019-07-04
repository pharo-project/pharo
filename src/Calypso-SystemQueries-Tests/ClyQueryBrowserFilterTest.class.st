Class {
	#name : #ClyQueryBrowserFilterTest,
	#superclass : #ClyItemFilterTestCase,
	#category : #'Calypso-SystemQueries-Tests-Queries'
}

{ #category : #helpers }
ClyQueryBrowserFilterTest >> itemOfThisTestClass [
	^ClyBrowserItem with: self class
]

{ #category : #helpers }
ClyQueryBrowserFilterTest >> itemOfThisTestMethod [
	^ClyBrowserItem with: self class >> testSelector
]

{ #category : #'tests-filter classes' }
ClyQueryBrowserFilterTest >> testMatchesClassOfPackageWhichMatchesIt [

	filter := ClyQueryBrowserFilter substringPattern: 'SystemQueries-Tests'.
	
	self assert: (filter matches: self itemOfThisTestClass)
]

{ #category : #'tests-filter classes' }
ClyQueryBrowserFilterTest >> testMatchesClassWhichNameMatchesIt [

	filter := ClyQueryBrowserFilter substringPattern: 'FilterTest'.
	
	self assert: (filter matches: self itemOfThisTestClass)
]

{ #category : #'tests-filter methods' }
ClyQueryBrowserFilterTest >> testMatchesMethodOfClassWhichMatchesIt [

	filter := ClyQueryBrowserFilter substringPattern: 'FilterTest'.
	
	self assert: (filter matches: self itemOfThisTestMethod)
]

{ #category : #'tests-filter methods' }
ClyQueryBrowserFilterTest >> testMatchesMethodOfPackageWhichMatchesIt [

	filter := ClyQueryBrowserFilter substringPattern: 'SystemQueries-Tests'.
	
	self assert: (filter matches: self itemOfThisTestMethod)
]

{ #category : #'tests-filter methods' }
ClyQueryBrowserFilterTest >> testMatchesMethodWithSelectorWhichMatchesIt [

	filter := ClyQueryBrowserFilter substringPattern: 'MethodWithSelector'.
	
	self assert: (filter matches: self itemOfThisTestMethod)
]

{ #category : #'tests-filter classes' }
ClyQueryBrowserFilterTest >> testNotMatchesClassWithWrongNameAndPackage [

	filter := ClyQueryBrowserFilter substringPattern: 'filter not matches class name and package'.
	
	self deny: (filter matches: self itemOfThisTestClass)
]

{ #category : #'tests-filter methods' }
ClyQueryBrowserFilterTest >> testNotMatchesMethodWithWrongClassAndPackageAndSelector [

	filter := ClyQueryBrowserFilter substringPattern: 'filter not matches package, class or selector'.
	
	self deny: (filter matches: self itemOfThisTestMethod)
]

{ #category : #'tests-filter by script' }
ClyQueryBrowserFilterTest >> testUsingBadScriptWhichNotReturnBoolean [

	filter := ClyQueryBrowserFilter substringPattern: '[:each | each selector]'.
	
	self assert: (filter matches: self itemOfThisTestMethod).
	self assert: (filter matches: (ClyBrowserItem with: Object >> #printString)).
]

{ #category : #'tests-filter by script' }
ClyQueryBrowserFilterTest >> testUsingBadScriptWithoutClosingBracket [

	filter := ClyQueryBrowserFilter substringPattern: '[:each | true '.
	
	self assert: (filter matches: self itemOfThisTestMethod).
	self assert: (filter matches: (ClyBrowserItem with: Object >> #printString)).
]

{ #category : #'tests-filter by script' }
ClyQueryBrowserFilterTest >> testUsingScript [

	filter := ClyQueryBrowserFilter substringPattern: '[:each | each selector = #testUsingScript]'.
	
	self assert: (filter matches: self itemOfThisTestMethod).
	self deny: (filter matches: (ClyBrowserItem with: Object >> #printString)).
]

{ #category : #'tests-filter by script' }
ClyQueryBrowserFilterTest >> testUsingUncompiledScript [

	filter := ClyQueryBrowserFilter substringPattern: '[:each | each 123selector]'.
	
	self assert: (filter matches: self itemOfThisTestMethod).
	self assert: (filter matches: (ClyBrowserItem with: Object >> #printString)).
]
