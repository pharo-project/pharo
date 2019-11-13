Class {
	#name : #SpCommandTest,
	#superclass : #TestCase,
	#instVars : [
		'command'
	],
	#category : #'Spec2-Commander2-Tests'
}

{ #category : #helpers }
SpCommandTest >> banana [
	"Does not return an icon, it is not needed for the test."
	^ 42
]

{ #category : #helpers }
SpCommandTest >> iconNamed: aSymbol [
	aSymbol ~= #banana
		ifTrue: [ self error: 'Only #banana icon is needed for tests.' ].
		
	^ self banana
]

{ #category : #running }
SpCommandTest >> setUp [
	super setUp.
	command := SpCommand new
]

{ #category : #test }
SpCommandTest >> testAsButtonPresenter [
	| buttonPresenter |
	command := CmCommand forSpec
		name: 'foo';
		description: 'bar';
		yourself.
	buttonPresenter := command asButtonPresenter.
	self assert: buttonPresenter label equals: 'foo'.
	self assert: buttonPresenter help equals: 'bar'.
	self assert: buttonPresenter icon isNil
]

{ #category : #test }
SpCommandTest >> testAsButtonPresenter2 [
	| buttonPresenter |
	command := CmCommand forSpec
		name: 'foo';
		description: 'bar';
		iconName: #blank;
		yourself.
	buttonPresenter := command asButtonPresenter.
	self assert: buttonPresenter label equals: 'foo'.
	self assert: buttonPresenter help equals: 'bar'.
	self
		assert: buttonPresenter icon
		equals: (buttonPresenter iconNamed: #blank)
]

{ #category : #test }
SpCommandTest >> testAsButtonPresenterExecutesOnClick [
	| flag buttonPresenter |
	flag := false.
	command := CmBlockCommand new
					block: [ flag := true ];
					yourself.
	
	buttonPresenter := command asSpecCommand asButtonPresenter.
	
	buttonPresenter click.
	
	self assert: flag equals: true
]

{ #category : #test }
SpCommandTest >> testHasIcon [
	self deny: command hasIcon.
	
	command iconName: #banana.
	
	self assert: command hasIcon
]

{ #category : #test }
SpCommandTest >> testHasShortcutKey [
	self deny: command hasShortcutKey.
	
	command shortcutKey: $a asKeyCombination.
	
	self assert: command hasShortcutKey.
]

{ #category : #test }
SpCommandTest >> testIcon [
	self assert: command icon isNil.
	
	command iconName: #add.
	
	self assert: command icon equals: (command iconNamed: #add).
]

{ #category : #test }
SpCommandTest >> testIconName [
	self assert: command iconName isNil.
	
	command iconName: #add.
	
	self assert: command iconName equals: #add
	
	
]

{ #category : #test }
SpCommandTest >> testIconNameFrom [
	self assert: command iconProvider equals: Smalltalk ui icons.
	
	command iconName: #banana from: self.
	
	self assert: command iconProvider equals: self.
	self assert: (command icon) equals: self banana
]

{ #category : #test }
SpCommandTest >> testIconProvider [
	self assert: command iconProvider equals: Smalltalk ui icons.
	
	command iconProvider: self.
	
	self assert: command iconProvider equals: self.
	
	command iconName: #banana.
	self assert: (command icon) equals: self banana
]

{ #category : #test }
SpCommandTest >> testShortcutKey [
	self should: [ command shortcutKey ] raise: CmNoShortcutIsDefined.
	
	command shortcutKey: $a asKeyCombination.
	
	self assert: command shortcutKey equals:  $a asKeyCombination
]
