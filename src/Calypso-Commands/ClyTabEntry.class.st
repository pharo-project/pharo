"
I am a browser tool declared for the tabs of a Calypso command map: the tool is opened as a tab of the browsers whose context is described by my anchor.
	aCommandMap tabs add: ClyMethodCodeEditorToolMorph
The tool is a subclass of ClyBrowserToolMorph. It can refuse a context with its class-side #shouldBeActivatedInContext:.
I hold the tool class where commands entries hold their command class (#commandClass), so the overrides of ClyCommandInteraction, such as #remove:, apply to me too.
The guide doc/Calypso/Commands.md of the Pharo repository describes the tabs.
"
Class {
	#name : 'ClyTabEntry',
	#superclass : 'ClyCommandEntry',
	#category : 'Calypso-Commands',
	#package : 'Calypso-Commands'
}

{ #category : 'operations' }
ClyTabEntry >> createToolFor: aBrowser inContext: aBrowserContext [

	^ commandClass for: aBrowser inContext: aBrowserContext
]

{ #category : 'activation' }
ClyTabEntry >> isActiveInContext: aBrowserContext [
	"The context must be described by my anchor and accepted by my tool."

	^ (self isVisibleInContext: aBrowserContext) and: [ commandClass shouldBeActivatedInContext: aBrowserContext ]
]

{ #category : 'accessing' }
ClyTabEntry >> toolClass [

	^ commandClass
]
