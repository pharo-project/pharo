"
This class is testing Monticello initialization
"
Class {
	#name : #MCWorkingCopyBrowserTest,
	#superclass : #TestCase,
	#instVars : [
		'browser'
	],
	#category : #'MonticelloGUI-Tests'
}

{ #category : #running }
MCWorkingCopyBrowserTest >> setUp [
	super setUp.
	browser := MCWorkingCopyBrowser new.
]

{ #category : #tests }
MCWorkingCopyBrowserTest >> testDefault [
	"When opened, there are plenty of packages, ie., working copies that are listed"
	self assert: browser workingCopies notNil.
	self assert: browser workingCopies notEmpty.
	
	"When opening, there is no repository selected"
	self assert: browser repository isNil.
	
	"No repository is selected"
	self assert: browser repositorySelection equals: 0
]

{ #category : #tests }
MCWorkingCopyBrowserTest >> testRepositories [
	self assert: browser repositories notNil.
	self assert: browser repositories notEmpty.
	self assert: browser repositoryGroup notNil.
	self assert: browser repositoryGroup repositories size equals: browser repositories size. 
]

{ #category : #tests }
MCWorkingCopyBrowserTest >> testRepositoriesIncludeCache [
	"At least one cache is present in the list of repositories"
	self assert: (browser repositories anySatisfy: [ :r | r class == MCCacheRepository ])
]

{ #category : #tests }
MCWorkingCopyBrowserTest >> testRepositoryList [
	self assert: browser repositoryList notNil.
	self assert: browser repositoryList notEmpty. 
	self assert: browser repositoryList size equals: browser repositories size.
	self assert: (browser repositoryList allSatisfy: [ :r | r isString ]).
]
