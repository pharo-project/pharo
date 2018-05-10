Class {
	#name : #MCWorkingCopyBrowserTest,
	#superclass : #TestCase,
	#instVars : [
		'b'
	],
	#category : #'MonticelloGUI-Tests'
}

{ #category : #running }
MCWorkingCopyBrowserTest >> setUp [
	super setUp.
	b := MCWorkingCopyBrowser new.
]

{ #category : #tests }
MCWorkingCopyBrowserTest >> testDefault [
	"When opened, there are plenty of packages, ie., working copies that are listed"
	self assert: b workingCopies notNil.
	self assert: b workingCopies notEmpty.
	
	"When opening, there is no repository selected"
	self assert: b repository isNil.
	
	"No repository is selected"
	self assert: b repositorySelection equals: 0
]

{ #category : #tests }
MCWorkingCopyBrowserTest >> testRepositories [
	self assert: b repositories notNil.
	self assert: b repositories notEmpty.
	self assert: b repositoryGroup notNil.
	self assert: b repositoryGroup repositories size equals: b repositories size. 
]

{ #category : #tests }
MCWorkingCopyBrowserTest >> testRepositoriesIncludeCache [
	"At least one cache is present in the list of repositories"
	self assert: (b repositories anySatisfy: [ :r | r class == MCCacheRepository ])
]

{ #category : #tests }
MCWorkingCopyBrowserTest >> testRepositoryList [
	self assert: b repositoryList notNil.
	self assert: b repositoryList notEmpty. 
	self assert: b repositoryList size equals: b repositories size.
	self assert: (b repositoryList allSatisfy: [ :r | r isString ]).
]
