"
I am a refactoring change for a method removement. I know about the class of this method, the selector and whether it is from the 
class or instance side. On removing this method (selector) will be removed from the class.
"
Class {
	#name : #RBRemoveMethodChange,
	#superclass : #RBRefactoryClassChange,
	#instVars : [
		'selector'
	],
	#category : #'Refactoring-Changes'
}

{ #category : #'instance creation' }
RBRemoveMethodChange class >> remove: aSymbol from: aClass [
	^ self new
		changeClass: aClass;
		selector: aSymbol;
		yourself
]

{ #category : #comparing }
RBRemoveMethodChange >> = aRemoveMethodChange [
	super = aRemoveMethodChange
		ifFalse: [ ^ false ].
	^ selector = aRemoveMethodChange selector
]

{ #category : #converting }
RBRemoveMethodChange >> asUndoOperation [
	^ changeFactory addMethodSource: (self methodSourceFor: selector) in: self changeClass
]

{ #category : #printing }
RBRemoveMethodChange >> changeString [
	^ 'Remove <1s>>>#<2s>'
		expandMacrosWith: self displayClassName
		with: selector
]

{ #category : #comparing }
RBRemoveMethodChange >> hash [
	^ selector hash
]

{ #category : #private }
RBRemoveMethodChange >> primitiveExecute [
	^ self changeClass removeSelector: selector
]

{ #category : #printing }
RBRemoveMethodChange >> printOn: aStream [ 
	aStream
		nextPutAll: self displayClassName;
		nextPutAll: ' removeSelector: ';
		print: self selector;
		nextPut: $!
]

{ #category : #private }
RBRemoveMethodChange >> selector [
	^ selector
]

{ #category : #initialization }
RBRemoveMethodChange >> selector: aSymbol [
	selector := aSymbol
]
