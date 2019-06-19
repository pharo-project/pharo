"
I represent any annotation user from particular class hierarchy.

I am hidden from programmers. They just specify the class of annotation user when they create annotations:
	MyClassAnnotation for: MyUserClass
Internally given class is converted to my instance:
	MyUserClass asAnnotationContext

When annotations are queried for particular user instance the result will include all annotations defined for given user class and its subclasses.	
I provide little hook to redefine this rule by annotation user. Instead of simple isKindOf check:
	anAnnotationUser isKindOf: annotationUserClass 
I ask given user if it represents simple annotation user:
	anAnnotationUser representsSimpleAnnotationUser: annotationUserClass
And by default objects just performs isKindOf: check to implement it.

To create my instance manually use:
	SimpleAnnotationContext of: MyUserClass

I define comparison methods in the way that two contexts with same user class will be equal.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	annotationUserClass:		<Class>
"
Class {
	#name : #SimpleAnnotationContext,
	#superclass : #AnnotationContext,
	#instVars : [
		'annotationUserClass'
	],
	#category : #ClassAnnotation
}

{ #category : #'instance creation' }
SimpleAnnotationContext class >> annotationUserClass: aClass [
	^self new 
		annotationUserClass: aClass
]

{ #category : #'instance creation' }
SimpleAnnotationContext class >> of: anAnnotationUserClass [
	^self new 
		annotationUserClass: anAnnotationUserClass
]

{ #category : #comparing }
SimpleAnnotationContext >> = anObject [
	"Answer whether the receiver and anObject represent the same object."

	self == anObject
		ifTrue: [ ^ true ].
	self class = anObject class
		ifFalse: [ ^ false ].
	^ annotationUserClass = anObject annotationUserClass
]

{ #category : #accessing }
SimpleAnnotationContext >> annotationUserClass [
	^ annotationUserClass
]

{ #category : #accessing }
SimpleAnnotationContext >> annotationUserClass: aClass [
	annotationUserClass := aClass
]

{ #category : #testing }
SimpleAnnotationContext >> describes: anAnnotationUser [
	"Following indirection provides extra flexibility for the users 
	to define special kind of simple application contexts
	and in same time to be able use simple classes to declare annotations.
	By default objects just performs isKindOf: check with my annotationUserClass"
	
	^anAnnotationUser representsSimpleAnnotationUser: annotationUserClass
]

{ #category : #comparing }
SimpleAnnotationContext >> hash [
	"Answer an integer value that is related to the identity of the receiver."

	^ annotationUserClass hash
]

{ #category : #printing }
SimpleAnnotationContext >> printOn: aStream [
	super printOn: aStream.
	
	aStream 
		nextPut: $(;
		nextPutAll: annotationUserClass name;
		nextPut: $)
]
