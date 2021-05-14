Class {
	#name : #ClyRestUntaggedClassesQueryTest,
	#superclass : #ClyClassQueryTest,
	#category : #'Calypso-SystemQueries-Tests-Queries'
}

{ #category : #running }
ClyRestUntaggedClassesQueryTest >> createQuery [
	^ClyRestUntaggedClassesQuery from: ClyPackageScope of: Object package in: environment
]

{ #category : #tests }
ClyRestUntaggedClassesQueryTest >> testEmptyInPackageWithoutAnyTags [
	query scope: (ClyPackageScope 
		of: ClyClass9FromPExtendedByP5Mock package in: environment).
		
	self assert: query checkEmptyResult
]

{ #category : #tests }
ClyRestUntaggedClassesQueryTest >> testFromPackageWithExtensionsButWithoutTags [
	
	self queryFromScope: ClyPackageScope of: ClyClass1FromP1Mock package.

	self assert: resultItems isEmpty
]

{ #category : #tests }
ClyRestUntaggedClassesQueryTest >> testFromPackageWithTagsAndUncategorizedClasses [
	
	self queryFromScope: ClyPackageScope of: ClyClass6UntaggedFromP5Mock package.

	self assert: resultItems asSet equals: {ClyClass6UntaggedFromP5Mock} asSet
]

{ #category : #tests }
ClyRestUntaggedClassesQueryTest >> testFromPackageWithoutTagsAndExtensions [
	
	self queryFromScope: ClyPackageScope of: ClyClass9FromPExtendedByP5Mock package.

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
		of: ClyClass6UntaggedFromP5Mock package in: environment).
		
	self deny: query checkEmptyResult
]

{ #category : #tests }
ClyRestUntaggedClassesQueryTest >> testNotSelectsTaggedClass [
	
	self deny: (query selectsClass: ClyClass7WithTag1FromP5Mock)
]

{ #category : #tests }
ClyRestUntaggedClassesQueryTest >> testSelectsClassWithoutTags [
	
	self assert: (query selectsClass: ClyClass6UntaggedFromP5Mock)
]
