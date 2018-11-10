"
I return all failed tests from given test cases available in my scope
"
Class {
	#name : #ClyFailedTestMethods,
	#superclass : #ClyMethodQuery,
	#category : #'Calypso-SystemPlugins-SUnit-Queries'
}

{ #category : #testing }
ClyFailedTestMethods >> buildResult: aQueryResult [
	| failedTests |
	failedTests := OrderedCollection new.
	scope classesDo: [ :each | each isTestCase ifTrue: [
		(each history at: #failures) do: [ :testSelector |
			(each lookupSelector: testSelector) ifNotNil: [ :method | 
				failedTests add: method ]].
		(each history at: #errors) do: [ :testSelector |
			(each lookupSelector: testSelector) ifNotNil: [ :method | 
				failedTests add: method ]].
	]].
	aQueryResult fillWith: failedTests
]

{ #category : #execution }
ClyFailedTestMethods >> checkEmptyResult [

	scope classesDo: [ :each | 
		(each isTestCase and: [each isAbstract not
			and: [ each hasFailedTest or: [ each hasErrorTest ] ]])
				ifTrue: [ ^false ]].
	^true
]

{ #category : #'item group decoration' }
ClyFailedTestMethods >> decorateItemGroup: methodGroupItem [
	| property |
	super decorateItemGroup: methodGroupItem.
	
	property := 	ClyTestResultProperty new.
	scope classesDo: [:testCase | | failures errors |
		(testCase isTestCase and: [testCase isAbstract not]) ifTrue: [ 
			failures := (testCase history at: #failures) size.
			errors := (testCase history at: #errors) size.
			property accumulateFailures: failures andErrors: errors]
	].	
	methodGroupItem addProperty: property
]

{ #category : #printing }
ClyFailedTestMethods >> description [

	^'failed tests'
]

{ #category : #testing }
ClyFailedTestMethods >> selectsMethod: aMethod [
	
	^aMethod isTestMethod and: [aMethod isPassedTest not]
]
