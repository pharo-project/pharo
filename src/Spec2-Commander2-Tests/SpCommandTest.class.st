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
SpCommandTest >> testCanUpdateEnableStatus [
	| buttonPresenter shouldBeAvailable |
	shouldBeAvailable := true.
	command := CmBlockCommand new
		canBeExecutedBlock: [ :ctx | shouldBeAvailable ];
		context: #foo; "need a conext for #canBeExecuted to be performed"
		yourself.
	command := command asSpecCommand.
	buttonPresenter := command asButtonPresenter.
	self assert: buttonPresenter isEnabled.
	
	shouldBeAvailable := false.
	command updateEnableStatus.
	
	self deny: buttonPresenter isEnabled
]

{ #category : #test }
SpCommandTest >> testConfigureAsButton [
	| button |
	command := (CmBlockCommand new
		name: 'foo';
		description: 'bar';
		block: [ 42 ];
		yourself) asSpecCommand.
		
	button := command
					configureAsButton;
					buildPresenter.
	
	self assert: button class equals: SpButtonPresenter.
	self assert: button label equals: command name.
	self assert: button help equals: command description.
	self assert: button icon isNil.
	self assert: button action value equals: command execute
]

{ #category : #test }
SpCommandTest >> testConfigureAsButtonWithIcon [
	| button |
	command := CmCommand forSpec
					iconName: #arrowUp;
					yourself.
		
	button := command
					configureAsButton;
					buildPresenter.
					
	self assert: button icon equals: command icon
]

{ #category : #test }
SpCommandTest >> testConfigureAsToolBarButton [
	| button |
	command := (CmBlockCommand new
		name: 'foo';
		description: 'bar';
		block: [ 42 ];
		yourself) asSpecCommand.
		
	button := command
					configureAsToolBarButton;
					buildPresenter.
	
	self assert: button class equals: SpToolBarButtonPresenter.
	self assert: button label equals: command name.
	self assert: button help equals: command description.
	self assert: button icon isNil.
	self assert: button action value equals: command execute
]

{ #category : #test }
SpCommandTest >> testConfigureAsToolBarToggleButton [
	| button |
	command := (CmBlockCommand new
		name: 'foo';
		description: 'bar';
		block: [ 42 ];
		yourself) asSpecCommand.
		
	button := command
					configureAsToolBarToggleButton;
					buildPresenter.
	
	self assert: button class equals: SpToolBarToggleButtonPresenter.
	self assert: button label equals: command name.
	self assert: button help equals: command description.
	self assert: button icon isNil.
	self assert: button action value equals: command execute
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
