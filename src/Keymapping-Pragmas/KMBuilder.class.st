"
I am a keymap builder that is used when configuring keymappings by pragmas.

Send me the #shortcut: message with a keymap name, so I give you a specific builder to build a keymap.

You can also tell me to install a category as global.
"
Class {
	#name : #KMBuilder,
	#superclass : #Object,
	#instVars : [
		'platform'
	],
	#category : #Keymapping-Pragmas
}

{ #category : #'instance creation' }
KMBuilder class >> keymap [
	^self keymap: #all
]

{ #category : #'instance creation' }
KMBuilder class >> keymap: aPlatform [
	(#( all Unix MacOSX Windows ) includes: aPlatform) ifFalse: [ self error: aPlatform, ' is not a valid platform' ].
	^self new
		platform: aPlatform;
		yourself
]

{ #category : #accessing }
KMBuilder >> platform: aPlatform [
	platform := aPlatform
]

{ #category : #keymap-building }
KMBuilder >> setAsGlobalCategory: aGlobalCategory [

	KMRepository default setAsGlobalCategory: aGlobalCategory
]

{ #category : #keymap-building }
KMBuilder >> shortcut: aKeymapName [
	^KMKeymapBuilder 
		for: aKeymapName
		platform: platform
]
