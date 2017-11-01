"
It's difficult to test whether menus appear because the test might block the UI thread and prevent it.

I use Morphic stepping to monitor the world for a new menu. If I find one, you can get it by sending me #menu.
"
Class {
	#name : #MenuCapturingMorph,
	#superclass : #Morph,
	#instVars : [
		'menu'
	],
	#category : 'SUnit-UITesting'
}

{ #category : #menu }
MenuCapturingMorph >> menu [

	^ menu.
]

{ #category : #'stepping and presenter' }
MenuCapturingMorph >> step [

	menu := World submorphs detect: [ :m | m isKindOf: MenuMorph ] ifNone: [ menu ].
	menu isNil ifFalse: [ 
		menu delete.
		self delete ].
]
