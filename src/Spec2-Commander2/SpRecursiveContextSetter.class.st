"
Visit recursively the tree of commands groups to set the context stored in my #context inst. var. to all CmCommands located at the leaves of the tree.
"
Class {
	#name : #SpRecursiveContextSetter,
	#superclass : #CmVisitor,
	#instVars : [
		'context'
	],
	#category : #'Spec2-Commander2-Visitors'
}

{ #category : #'instance creation' }
SpRecursiveContextSetter class >> context: context [
	^ self new
		context: context;
		yourself
]

{ #category : #helper }
SpRecursiveContextSetter class >> visit: aCommandOrGroup toSetContext: anObject [
	aCommandOrGroup
		acceptVisitor: (self context: anObject).
		
	^ aCommandOrGroup
]

{ #category : #accessing }
SpRecursiveContextSetter >> context [
	^ context
]

{ #category : #accessing }
SpRecursiveContextSetter >> context: anObject [
	context := anObject
]

{ #category : #visiting }
SpRecursiveContextSetter >> visitCommand: aCmCommand [
	aCmCommand context
		ifNotNil: [ ^ self ].
	
	aCmCommand
		context: self context
]
