"
I represent undefined annotation context which is initialized by default for annotation instances.

Annotations with undefined contexts (unspecified context) are never returned from contextual query of annotations.

I has singleton #instance on class side which is used by annotations as default context value
"
Class {
	#name : #NullAnnotationContext,
	#superclass : #AnnotationContext,
	#classInstVars : [
		'instance'
	],
	#category : #ClassAnnotation
}

{ #category : #accessing }
NullAnnotationContext class >> instance [
	^instance ifNil: [instance := self new]
]

{ #category : #testing }
NullAnnotationContext >> describes: anAnnotationUser [

	^false
]
