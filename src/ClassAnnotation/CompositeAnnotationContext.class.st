"
I am composite context. I represent users of annotations which satisfy all my parts.

To create my instances use following method:

	CompositeAnnotationContext with: {aContext1. aContext2}
	
Or use comma message to concatenate other contexts: 

	aContext1 , aContext2

Internal Representation and Key Implementation Points.

    Instance Variables
	parts:		<Collection of<AnnotationContext>>
"
Class {
	#name : #CompositeAnnotationContext,
	#superclass : #AnnotationContext,
	#instVars : [
		'parts'
	],
	#category : #ClassAnnotation
}

{ #category : #'instance creation' }
CompositeAnnotationContext class >> with: contextCollection [
	^self new 
		parts: contextCollection 
]

{ #category : #converting }
CompositeAnnotationContext >> , anAnnotationContext [
	^ CompositeAnnotationContext with: (parts copyWith: anAnnotationContext asAnnotationContext)
]

{ #category : #comparing }
CompositeAnnotationContext >> = anObject [
	"Answer whether the receiver and anObject represent the same object."

	self == anObject ifTrue: [ ^ true ].
	self class = anObject class ifFalse: [ ^ false ].
	
	^ parts = anObject parts
]

{ #category : #testing }
CompositeAnnotationContext >> describes: anAnnotationUser [
	^parts allSatisfy: [ :each | each describes: anAnnotationUser ]
]

{ #category : #comparing }
CompositeAnnotationContext >> hash [
	"Answer an integer value that is related to the identity of the receiver."

	^ parts hash
]

{ #category : #accessing }
CompositeAnnotationContext >> parts [
	^ parts
]

{ #category : #accessing }
CompositeAnnotationContext >> parts: anObject [
	parts := anObject
]

{ #category : #printing }
CompositeAnnotationContext >> printOn: aStream [
	super printOn: aStream.
	
	aStream nextPut: $(.
	parts do: [ :each | 
		each printOn: aStream.
		aStream nextPutAll: ', ' ].
	aStream skip: -1; nextPut: $)
]
