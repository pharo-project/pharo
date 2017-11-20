Class {
	#name : #AbstractObjectsAsMethod,
	#superclass : #Object,
	#category : #Tests-ObjectsAsMethods
}

{ #category : #compatibility }
AbstractObjectsAsMethod >> flushCache [
]

{ #category : #compatibility }
AbstractObjectsAsMethod >> methodClass: aMethodClass [
]

{ #category : #compatibility }
AbstractObjectsAsMethod >> origin [
	^self class
]

{ #category : #compatibility }
AbstractObjectsAsMethod >> package [
	^self class package
]

{ #category : #compatibility }
AbstractObjectsAsMethod >> pragmas [
	^#()
]

{ #category : #compatibility }
AbstractObjectsAsMethod >> selector: aSymbol [
]
