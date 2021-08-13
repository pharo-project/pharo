"
A CBResetWindowTest is a test class for testing the behaivor of CBResetWindowTest
"
Class {
	#name : #CBResetWindowTest,
	#superclass : #TestCase,
	#instVars : [
		'resetWindow'
	],
	#category : #'Tool-CriticBrowser-Tests'
}

{ #category : #running }
CBResetWindowTest >> setUp [

	super setUp.
	resetWindow := CBResetWindow new
]

{ #category : #tests }
CBResetWindowTest >> testInitializePresenters [
self
		assert: resetWindow resetModel class
		equals: SpButtonPresenter.
	self
		assert: resetWindow currentConfigurationModel class
		equals: SpButtonPresenter.
	
]

{ #category : #tests }
CBResetWindowTest >> testOpen [

	| resetWindow |
	resetWindow := CBResetWindow open.
	self assert: resetWindow isBuilt.
	resetWindow close
]

{ #category : #tests }
CBResetWindowTest >> testOpenPackagesChooser [

	| applier |
	applier := resetWindow openPackagesChooser.
	self assert: applier isBuilt.
	applier window close
]
