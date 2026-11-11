"
I am a command declared for the shortcuts of a Calypso command map, with the key combination which runs it:
	aCommandMap shortcuts add: SycRemoveMethodCommand key: $x meta
When the keys typed in a tool match my key combination, ClyKMDispatcher runs my command in the context of the tool, as ClyShortcutActivation does.
"
Class {
	#name : 'ClyShortcutCommandEntry',
	#superclass : 'ClyCommandEntry',
	#instVars : [
		'keyCombination'
	],
	#category : 'Calypso-Commands',
	#package : 'Calypso-Commands'
}

{ #category : 'shortcuts browser' }
ClyShortcutCommandEntry >> action [
	"The method to browse from the shortcuts browser of NewTools, as ClyShortcutActivation>>#action."

	^ declaringMethod
]

{ #category : 'shortcuts browser' }
ClyShortcutCommandEntry >> description [

	^ declaringMethod comment ifNil: [ String empty ]
]

{ #category : 'executing' }
ClyShortcutCommandEntry >> executeCommandInContext: aToolContext [

	commandClass new
		context: aToolContext;
		execute
]

{ #category : 'accessing' }
ClyShortcutCommandEntry >> keyCombination [

	^ keyCombination
]

{ #category : 'accessing' }
ClyShortcutCommandEntry >> keyCombination: aKeyCombination [

	keyCombination := aKeyCombination
]

{ #category : 'shortcuts browser' }
ClyShortcutCommandEntry >> name [

	^ commandClass name asString
]

{ #category : 'shortcuts browser' }
ClyShortcutCommandEntry >> scope [
	"The package of my command, as ClyShortcutActivation>>#scope."

	^ commandClass packageName
]

{ #category : 'shortcuts browser' }
ClyShortcutCommandEntry >> scopeName [

	^ self scope asString
]

{ #category : 'shortcuts browser' }
ClyShortcutCommandEntry >> shortcut [

	^ keyCombination asString
]

{ #category : 'executing' }
ClyShortcutCommandEntry >> tryExecuteCommandInContext: aToolContext byEvents: anEventBuffer [
	"Answers whether the typed keys are my key combination or its beginning, as ClyShortcutActivation does. My command runs when they are the whole combination."

	(keyCombination matches: anEventBuffer) ifFalse: [ ^ false ].
	(self isActiveInContext: aToolContext) ifFalse: [ ^ false ].
	(keyCombination matchesCompletely: anEventBuffer)
		ifTrue: [
				KMBuffer uniqueInstance completeMatch.
				self executeCommandInContext: aToolContext ]
		ifFalse: [ KMBuffer uniqueInstance partialMatch ].
	^ true
]
