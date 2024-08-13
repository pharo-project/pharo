"
I am a class refactoring change for removing a class. On execution, my class will be removed from the system (all globals).
"
Class {
	#name : 'RBRemoveClassChange',
	#superclass : 'RBRefactoryClassChange',
	#category : 'Refactoring-Changes-Changes-Classes',
	#package : 'Refactoring-Changes',
	#tag : 'Changes-Classes'
}

{ #category : 'instance creation' }
RBRemoveClassChange class >> remove: aClass [

	^ self new changeClass: aClass
]

{ #category : 'instance creation' }
RBRemoveClassChange class >> removeClassName: aSymbol [

	^ self basicNew
		  onSystemEnvironment: self environment;
		  changeClassName: aSymbol;
		  initialize
]

{ #category : 'converting' }
RBRemoveClassChange >> asUndoOperation [

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
RBRemoveClassChange >> changeClass: aBehavior [

	super changeClass: aBehavior.
	isMeta := false
]

{ #category : 'printing' }
RBRemoveClassChange >> changeString [

	^ 'Remove ' , self displayClassName
]

{ #category : 'private' }
RBRemoveClassChange >> generateChanges [

	self changeClass removeFromSystem
]

{ #category : 'initialization' }
RBRemoveClassChange >> initialize [

	super initialize.
	changeFactory := RBRefactoryChangeManager changeFactory
]

{ #category : 'printing' }
RBRemoveClassChange >> printOn: aStream [

	aStream
		nextPutAll: self displayClassName;
		nextPutAll: ' removeFromSystem'
]
