"
I am a generic adapter used to embed directly a Morph in spec.

Be aware that when you use this, you broke Spec plateform independency and force you application to run only on top of Morphic
"
Class {
	#name : #MorphicGenericAdapter,
	#superclass : #AbstractMorphicAdapter,
	#category : #Spec-MorphicAdapters
}

{ #category : #'instance creation' }
MorphicGenericAdapter class >> morph: morph [

	^ self new
		morph: morph;
		yourself
]

{ #category : #accessing }
MorphicGenericAdapter >> morph [

	^ self widget
]

{ #category : #accessing }
MorphicGenericAdapter >> morph: anObject [

	widget := anObject
]
