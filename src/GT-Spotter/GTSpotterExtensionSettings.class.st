"
This is a helper class that provides dynamically setting entries for all processors defined in the image. The user can use the settings browser to turn each processor on/off.

To achieve this, the class side keeps track of disabledExtensions.

As each setting requires selectors for setting/getting values, the class side implements a doesNotUnderstand: and maintains the disabledExtensions list.
"
Class {
	#name : #GTSpotterExtensionSettings,
	#superclass : #Object,
	#classInstVars : [
		'disabledExtensions'
	],
	#category : #'GT-Spotter-Model'
}

{ #category : #private }
GTSpotterExtensionSettings class >> disabledExtensions [
	^ disabledExtensions ifNil: [ disabledExtensions := Set new ]
]

{ #category : #settings }
GTSpotterExtensionSettings class >> groupedAndSortedSpotterExtensions [
	"Return a list of associations in which:
		the key is a class
		the value is the list of spotter extensions.
	The returned list is ordered according to the class name"

	| extensions groupedExtensions |
	extensions := GTSpotter spotterExtendingMethods.
	groupedExtensions := (extensions groupedBy: #methodClass) associations.
	^ groupedExtensions asSortedCollection: [ :a1 :a2 | a1 key name < a2 key name ]. 
]

{ #category : #settings }
GTSpotterExtensionSettings class >> shouldSpotterIgnoreProcessorDefinedInMethod: aCompiledMethod [
	"
	Return a boolean value indicating if the spotter extension method, provided as argument, has to be disabled or not
	 
	self shouldSpotterIgnoreProcessorDefinedInMethod: (MCPackage>> #spotterForPackageFor:)
	"
	^ self disabledExtensions includes: aCompiledMethod methodClass name, '_', aCompiledMethod selector allButLast
]
