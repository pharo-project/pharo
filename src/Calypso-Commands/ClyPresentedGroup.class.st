"
I present one menu group of a Calypso menu tree built on Commander2. I wrap the menu group descriptor (a ClyMenuGroup or the root group) that names me; my order and display rules come from it. My #name is the unique identifier of the group inside its parent, and my #dynamicName is the label the menu shows. I am active when one of my entries is active.
"
Class {
	#name : 'ClyPresentedGroup',
	#superclass : 'CmCommandGroup',
	#instVars : [
		'descriptor'
	],
	#category : 'Calypso-Commands',
	#package : 'Calypso-Commands'
}

{ #category : 'accessing' }
ClyPresentedGroup >> descriptor [
	^ descriptor
]

{ #category : 'accessing' }
ClyPresentedGroup >> descriptor: aMenuGroup [
	descriptor := aMenuGroup
]

{ #category : 'accessing' }
ClyPresentedGroup >> dynamicName [
	^ descriptor name
]

{ #category : 'testing' }
ClyPresentedGroup >> isActive [
	^ self entries anySatisfy: [ :each | each isActive ]
]

{ #category : 'testing' }
ClyPresentedGroup >> isInlined [
	^ descriptor isInlined
]

{ #category : 'accessing' }
ClyPresentedGroup >> order [
	^ descriptor order
]
