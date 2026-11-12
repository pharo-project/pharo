"
Tests that the class annotation strategies of Calypso also answer the commands declared with the <calypsoCommands> pragma for their interaction, so Calypso shows both, and tests the entries of shortcuts, clicks and drag and drop.
"
Class {
	#name : 'ClyCommandActivationStrategyRegistryTest',
	#superclass : 'TestCase',
	#category : 'Commander-Calypso-Compatibility-Tests',
	#package : 'Commander-Calypso-Compatibility-Tests'
}

{ #category : 'helpers' }
ClyCommandActivationStrategyRegistryTest >> mouseEventWithButton: aButton [

	^ MouseButtonEvent basicNew
		  setType: #mouseDown
		  position: 0 @ 0
		  which: aButton
		  buttons: aButton
		  hand: nil
		  stamp: 0
]

{ #category : 'tests' }
ClyCommandActivationStrategyRegistryTest >> testActivationsIncludeRemainingClassAnnotations [

	| activations |
	activations := ClyCommandRegistry default activationsOf: #contextMenu inContext: ClyMockAnnotatedToolContext new.
	self assert: activations size equals: 1.
	self assert: activations first commandClass identicalTo: ClyMockAnnotatedCommand
]

{ #category : 'tests' }
ClyCommandActivationStrategyRegistryTest >> testDropEntryIsRunByADragAndDropActivator [

	| entry |
	entry := (ClyCommandMap forAnchor: ClyMockToolContext) dragAndDrop add: ClyMockRegisteredFirstCommand dropIn: ClyMockToolSubContext.
	self assert: (entry newActivatorFor: ClyMockToolContext new) class identicalTo: ClyDragAndDropCommandActivator
]

{ #category : 'tests' }
ClyCommandActivationStrategyRegistryTest >> testMenuOfStrategyShowsDeclaredCommands [

	| context root |
	context := ClyMockToolContext new.
	root := ClyCommandGroupBuilder new buildMenuFor: ClyContextMenuActivation context: context pureRoots: #(  ).
	self assert: (root entries collect: [ :entry | entry dynamicName ]) asArray equals: #( 'First' 'Extra' )
]

{ #category : 'tests' }
ClyCommandActivationStrategyRegistryTest >> testMouseEntryWaitsForItsButton [

	| entry |
	entry := (ClyCommandMap forAnchor: ClyMockToolContext) textLeftBarClick add: ClyMockRegisteredFirstCommand.
	self assert: (entry matchesEvent: (self mouseEventWithButton: MouseEvent redButton)).
	entry beYellowButton.
	self deny: (entry matchesEvent: (self mouseEventWithButton: MouseEvent redButton)).
	self assert: (entry matchesEvent: (self mouseEventWithButton: MouseEvent yellowButton))
]

{ #category : 'tests' }
ClyCommandActivationStrategyRegistryTest >> testStrategiesAnswerDeclaredEntriesOfTheirInteraction [

	| context |
	context := ClyMockToolContext new.
	self assert: ((ClyContextMenuActivation visibleInstancesInContext: context) collect: [ :entry | entry commandClass ]) asArray equals: {
			ClyMockRegisteredFirstCommand.
			ClyMockRegisteredSecondCommand }.
	self assert: ((ClyShortcutActivation activeInstancesInContext: context) collect: [ :entry | entry keyCombination ]) asArray equals: { $x meta }.
	self assertEmpty: (ClyToolbarActivation visibleInstancesInContext: context)
]

{ #category : 'tests' }
ClyCommandActivationStrategyRegistryTest >> testStrategyWithoutInteractionAnswersOnlyItsAnnotations [

	self assert: ClyMenuCommandActivationStrategy interactionName isNil.
	self assert: ClyMenuCommandActivationStrategy registeredInstances asArray equals: (ClyMenuCommandActivationStrategy registry allInstancesOf: ClyMenuCommandActivationStrategy) asArray
]
