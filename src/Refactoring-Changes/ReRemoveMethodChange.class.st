"
I am a refactoring change for a method removement. I know about the class of this method, the selector and whether it is from the 
class or instance side. On removing this method (selector) will be removed from the class.
"
Class {
	#name : 'ReRemoveMethodChange',
	#superclass : 'ReClassChange',
	#instVars : [
		'selector'
	],
	#category : 'Refactoring-Changes',
	#package : 'Refactoring-Changes'
}

{ #category : 'instance creation' }
ReRemoveMethodChange class >> remove: aSymbol from: aClass [

	^ self new
		changeClass: aClass;
		selector: aSymbol;
		yourself
]

{ #category : 'comparing' }
ReRemoveMethodChange >> = aRemoveMethodChange [

	super = aRemoveMethodChange
		ifFalse: [ ^ false ].
	^ selector = aRemoveMethodChange selector
]

{ #category : 'converting' }
ReRemoveMethodChange >> asUndoOperation [

	^ changeFactory
		  addMethodSource: (self methodSourceFor: selector)
		  in: self changeClass
]

{ #category : 'printing' }
ReRemoveMethodChange >> changeString [

	^ 'Remove <1s>>>#<2s>'
		expandMacrosWith: self displayClassName
		with: selector
]

{ #category : 'private' }
ReRemoveMethodChange >> generateChanges [

	^ self changeClass removeSelector: selector
]

{ #category : 'comparing' }
ReRemoveMethodChange >> hash [

	^ selector hash
]

{ #category : 'printing' }
ReRemoveMethodChange >> printOn: aStream [

	aStream
		nextPutAll: self displayClassName;
		nextPutAll: ' removeSelector: ';
		print: self selector
]

{ #category : 'private' }
ReRemoveMethodChange >> selector [

	^ selector
]

{ #category : 'initialization' }
ReRemoveMethodChange >> selector: aSymbol [

	selector := aSymbol
]
