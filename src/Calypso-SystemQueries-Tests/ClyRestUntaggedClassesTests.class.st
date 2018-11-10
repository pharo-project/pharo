Class {
	#name : #ClyRestUntaggedClassesTests,
	#superclass : #ClyClassQueryTestCase,
	#category : #'Calypso-SystemQueries-Tests-Queries'
}

{ #category : #running }
ClyRestUntaggedClassesTests >> createQuery [
	^ClyRestUntaggedClasses from: ClyPackageScope of: Object package in: environment
]

{ #category : #tests }
ClyRestUntaggedClassesTests >> testEmptyInPackageWithoutAnyTags [
	query scope: (ClyPackageScope 
		of: ClyClass9FromPExtendedByP5 package in: environment).
		
	self assert: query checkEmptyResult
]

{ #category : #tests }
ClyRestUntaggedClassesTests >> testFromPackageWithExtensionsButWithoutTags [
	
	self queryFromScope: ClyPackageScope of: ClyClass1FromP1 package.

	self assert: resultItems isEmpty
]

{ #category : #tests }
ClyRestUntaggedClassesTests >> testFromPackageWithTagsAndUncategorizedClasses [
	
	self queryFromScope: ClyPackageScope of: ClyClass6UntaggedFromP5 package.

	self assert: resultItems asSet equals: {ClyClass6UntaggedFromP5} asSet
]

{ #category : #tests }
ClyRestUntaggedClassesTests >> testFromPackageWithoutTagsAndExtensions [
	
	self queryFromScope: ClyPackageScope of: ClyClass9FromPExtendedByP5 package.

	self assert: resultItems isEmpty
]

{ #category : #tests }
ClyRestUntaggedClassesTests >> testIsEmptyFromEmptyClassScope [
	"This query supports only package scope"
	<expectedFailure>
	
	super testIsEmptyFromEmptyClassScope
]

{ #category : #tests }
ClyRestUntaggedClassesTests >> testNotEmptyInPackageWithTagsAndUncategorizedClasses [
	query scope: (ClyPackageScope 
		of: ClyClass6UntaggedFromP5 package in: environment).
		
	self deny: query checkEmptyResult
]

{ #category : #tests }
ClyRestUntaggedClassesTests >> testNotSelectsTaggedClass [
	
	self deny: (query selectsClass: ClyClass7WithTag1FromP5)
]

{ #category : #tests }
ClyRestUntaggedClassesTests >> testSelectsClassWithoutTags [
	
	self assert: (query selectsClass: ClyClass6UntaggedFromP5)
]
