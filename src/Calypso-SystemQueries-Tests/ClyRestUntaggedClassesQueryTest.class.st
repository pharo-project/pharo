Class {
	#name : #ClyRestUntaggedClassesQueryTest,
	#superclass : #ClyClassQueryTestCase,
	#category : #'Calypso-SystemQueries-Tests-Queries'
}

{ #category : #running }
ClyRestUntaggedClassesQueryTest >> createQuery [
	^ClyRestUntaggedClassesQuery from: ClyPackageScope of: Object package in: environment
]

{ #category : #tests }
ClyRestUntaggedClassesQueryTest >> testEmptyInPackageWithoutAnyTags [
	query scope: (ClyPackageScope 
		of: ClyClass9FromPExtendedByP5 package in: environment).
		
	self assert: query checkEmptyResult
]

{ #category : #tests }
ClyRestUntaggedClassesQueryTest >> testFromPackageWithExtensionsButWithoutTags [
	
	self queryFromScope: ClyPackageScope of: ClyClass1FromP1 package.

	self assert: resultItems isEmpty
]

{ #category : #tests }
ClyRestUntaggedClassesQueryTest >> testFromPackageWithTagsAndUncategorizedClasses [
	
	self queryFromScope: ClyPackageScope of: ClyClass6UntaggedFromP5 package.

	self assert: resultItems asSet equals: {ClyClass6UntaggedFromP5} asSet
]

{ #category : #tests }
ClyRestUntaggedClassesQueryTest >> testFromPackageWithoutTagsAndExtensions [
	
	self queryFromScope: ClyPackageScope of: ClyClass9FromPExtendedByP5 package.

	self assert: resultItems isEmpty
]

{ #category : #tests }
ClyRestUntaggedClassesQueryTest >> testIsEmptyFromEmptyClassScope [
	"This query supports only package scope"
	<expectedFailure>
	
	super testIsEmptyFromEmptyClassScope
]

{ #category : #tests }
ClyRestUntaggedClassesQueryTest >> testNotEmptyInPackageWithTagsAndUncategorizedClasses [
	query scope: (ClyPackageScope 
		of: ClyClass6UntaggedFromP5 package in: environment).
		
	self deny: query checkEmptyResult
]

{ #category : #tests }
ClyRestUntaggedClassesQueryTest >> testNotSelectsTaggedClass [
	
	self deny: (query selectsClass: ClyClass7WithTag1FromP5)
]

{ #category : #tests }
ClyRestUntaggedClassesQueryTest >> testSelectsClassWithoutTags [
	
	self assert: (query selectsClass: ClyClass6UntaggedFromP5)
]
