"
A CmUICommandTest is a test class for testing the behavior of CmUICommand
"
Class {
	#name : #CmUICommandTest,
	#superclass : #TestCase,
	#instVars : [
		'command'
	],
	#category : #'Commander2-UI-Tests-Core'
}

{ #category : #helpers }
CmUICommandTest >> banana [
	"Does not return an icon, it is not needed for the test."
	^ 42
]

{ #category : #helpers }
CmUICommandTest >> iconNamed: aSymbol [
	aSymbol ~= #banana
		ifTrue: [ self error: 'Only #banana icon is needed for tests.' ].
		
	^ self banana
]

{ #category : #running }
CmUICommandTest >> setUp [
	super setUp.
	command := CmUICommand new
]

{ #category : #tests }
CmUICommandTest >> testHasIcon [
	self deny: command hasIcon.
	
	command iconName: #banana.
	
	self assert: command hasIcon
]

{ #category : #tests }
CmUICommandTest >> testHasShortcutKey [
	self deny: command hasShortcutKey.
	
	command shortcutKey: $a asKeyCombination.
	
	self assert: command hasShortcutKey.
]

{ #category : #tests }
CmUICommandTest >> testIcon [
	self assert: command icon isNil.
	
	command iconName: #add.
	
	self assert: command icon equals: (command iconNamed: #add).
]

{ #category : #tests }
CmUICommandTest >> testIconName [
	self assert: command iconName isNil.
	
	command iconName: #add.
	
	self assert: command iconName equals: #add
	
	
]

{ #category : #tests }
CmUICommandTest >> testIconNameFrom [
	self assert: command iconProvider equals: Smalltalk ui icons.
	
	command iconName: #banana from: self.
	
	self assert: command iconProvider equals: self.
	self assert: (command icon) equals: self banana
]

{ #category : #tests }
CmUICommandTest >> testIconProvider [
	self assert: command iconProvider equals: Smalltalk ui icons.
	
	command iconProvider: self.
	
	self assert: command iconProvider equals: self.
	
	command iconName: #banana.
	self assert: (command icon) equals: self banana
]

{ #category : #tests }
CmUICommandTest >> testShortcutKey [
	self should: [ command shortcutKey ] raise: CmNoShortcutIsDefined.
	
	command shortcutKey: $a asKeyCombination.
	
	self assert: command shortcutKey equals:  $a asKeyCombination
]
