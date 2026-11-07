"
I present one command in a Calypso menu tree built on Commander2. I wrap either an activator (a command reached through an activation of a tool context) or a command that is already prepared for its context (a pure Commander2 command). My #name is an identifier that is unique inside my group; my #dynamicName is the label the menu shows, so labels may repeat, as they do in Calypso menus. I answer activity, description and execution by delegating to my activator or my command, so a presented command behaves like the v1 activator items did.
"
Class {
	#name : 'ClyPresentedCommand',
	#superclass : 'CmUICommand',
	#instVars : [
		'activator',
		'identifier'
	],
	#category : 'Calypso-Commands',
	#package : 'Calypso-Commands'
}

{ #category : 'accessing' }
ClyPresentedCommand >> activator [
	^ activator
]

{ #category : 'accessing' }
ClyPresentedCommand >> activator: anActivator [
	activator := anActivator
]

{ #category : 'testing' }
ClyPresentedCommand >> canBeExecuted [
	^ self isActive
]

{ #category : 'accessing' }
ClyPresentedCommand >> command [
	^ activator
		ifNil: [ self decoratedCommand ]
		ifNotNil: [ activator command ]
]

{ #category : 'accessing' }
ClyPresentedCommand >> description [
	^ activator
		ifNil: [ self decoratedCommand description ]
		ifNotNil: [ activator commandDescription ]
]

{ #category : 'accessing' }
ClyPresentedCommand >> dynamicName [
	^ activator
		ifNil: [ self decoratedCommand name ]
		ifNotNil: [ activator menuItemName ]
]

{ #category : 'executing' }
ClyPresentedCommand >> execute [
	activator
		ifNil: [ self decoratedCommand execute ]
		ifNotNil: [ activator executeCommand ]
]

{ #category : 'accessing' }
ClyPresentedCommand >> identifier [
	^ identifier
]

{ #category : 'accessing' }
ClyPresentedCommand >> identifier: aString [
	identifier := aString
]

{ #category : 'testing' }
ClyPresentedCommand >> isActive [
	^ activator
		ifNil: [ self decoratedCommand canBeExecuted ]
		ifNotNil: [ activator canExecuteCommand ]
]

{ #category : 'accessing' }
ClyPresentedCommand >> name [
	^ identifier
]

{ #category : 'accessing' }
ClyPresentedCommand >> order [
	^ activator
		ifNil: [ self decoratedCommand orderInContext: self decoratedCommand context ]
		ifNotNil: [ activator menuItemOrder ]
]
