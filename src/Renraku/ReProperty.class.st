"
I'm an external property of some entity related to a piece of code.

I define a basic interface of a #title and an #icon that can be used to display it in a user interface
I also have a source anchor with a reference to the actual class, method, or other source code entity that I am related to.
"
Class {
	#name : 'ReProperty',
	#superclass : 'Model',
	#instVars : [
		'sourceAnchor'
	],
	#category : 'Renraku-Critiques',
	#package : 'Renraku',
	#tag : 'Critiques'
}

{ #category : 'item convertation' }
ReProperty class >> convertToCalypsoBrowserItem: aProperty [
	^ClyBrowserItem named: aProperty title with: aProperty
]

{ #category : 'item decoration' }
ReProperty class >> decorateBrowserItem: aBrowserItem by: anEnvironmentPlugin [
	"Following code is to support Pharo 7 Exception property when there is problem with critiques.
	Pharo 6 has not this class. So this special trick is used"
	| property |
	property := aBrowserItem actualObject.
	property class name = #ReExceptionProperty ifTrue: [
		aBrowserItem markWith: ClyBrokenCritiqueTag]
]

{ #category : 'actions' }
ReProperty >> actions [

	^ #()
]

{ #category : 'accessing' }
ReProperty >> color [

	^ Color black alpha: 0.1
]

{ #category : 'accessing' }
ReProperty >> entity [
	^ self sourceAnchor entity
]

{ #category : 'accessing' }
ReProperty >> icon [

	^ Form extent: 16@16 depth: 32
]

{ #category : 'initialization' }
ReProperty >> initializeSourceAnchor: anAnchor [

	self initialize.
	sourceAnchor := anAnchor
]

{ #category : 'testing' }
ReProperty >> isCritique [
	^ false
]

{ #category : 'accessing' }
ReProperty >> sourceAnchor [
	^ sourceAnchor
]

{ #category : 'accessing' }
ReProperty >> title [

	^ self subclassResponsibility
]
