Class {
	#name : 'SycSuperclassImplementorsCommand',
	#superclass : 'SycChangeMessageSignatureCommand',
	#category : 'SystemCommands-MessageCommands',
	#package : 'SystemCommands-MessageCommands'
}

{ #category : 'execution' }
SycSuperclassImplementorsCommand >> applyResultInContext: a [
]

{ #category : 'accessing' }
SycSuperclassImplementorsCommand >> defaultMenuIconName [
	^#edit
]

{ #category : 'accessing' }
SycSuperclassImplementorsCommand >> defaultMenuItemName [
	^'Hierarchical implementors'
]

{ #category : 'execution' }
SycSuperclassImplementorsCommand >> execute [

	| sup |
	sup := refactoredClass withAllSuperclasses 
		detect: [ :cl | cl includesSelector: originalMessage selector ] 
		ifNone: [ ^ self ].
	Smalltalk tools messageList browse: { sup >> originalMessage selector }
]

{ #category : 'execution' }
SycSuperclassImplementorsCommand >> isComplexRefactoring [

	^ false
]
