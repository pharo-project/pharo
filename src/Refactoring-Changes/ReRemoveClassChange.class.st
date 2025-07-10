"
I am a class refactoring change for removing a class. On execution, my class will be removed from the system (all globals).
"
Class {
	#name : 'ReRemoveClassChange',
	#superclass : 'ReClassChange',
	#category : 'Refactoring-Changes',
	#package : 'Refactoring-Changes'
}

{ #category : 'instance creation' }
ReRemoveClassChange class >> remove: aClass [

	^ self new changeClass: aClass
]

{ #category : 'instance creation' }
ReRemoveClassChange class >> removeClassName: aSymbol [

	^ self basicNew
		  onSystemEnvironment: self environment;
		  changeClassName: aSymbol;
		  initialize
]

{ #category : 'converting' }
ReRemoveClassChange >> asUndoOperation [

	| classChanges |
	classChanges := changeFactory compositeRefactoryChange.
	self changeClass withAllSubclasses do: [ :class |
		classChanges defineClass: [ :aBuilder | aBuilder fillFor: class ].
		class class instVarNames do: [ :varName | classChanges addInstanceVariable: varName to: class class ].
		class selectors do: [ :selector | classChanges compile: (class sourceCodeAt: selector) in: class ].
		class class selectors do: [ :selector | classChanges compile: (class class sourceCodeAt: selector) in: class class ] ].
	^ classChanges
]

{ #category : 'accessing' }
ReRemoveClassChange >> changeClass: aBehavior [

	super changeClass: aBehavior.
	isMeta := false
]

{ #category : 'printing' }
ReRemoveClassChange >> changeString [

	^ 'Remove ' , self displayClassName
]

{ #category : 'private' }
ReRemoveClassChange >> generateChanges [

	self changeClass removeFromSystem
]

{ #category : 'initialization' }
ReRemoveClassChange >> initialize [

	super initialize.
	changeFactory := ReChangeManager changeFactory
]

{ #category : 'printing' }
ReRemoveClassChange >> printOn: aStream [

	aStream
		nextPutAll: self displayClassName;
		nextPutAll: ' removeFromSystem'
]
