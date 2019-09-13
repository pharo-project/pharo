"
I am a generic adapter used to embed directly a Morph in spec.

Be aware that when you use this, you broke Spec plateform independency and force you application to run only on top of Morphic
"
Class {
	#name : #SpMorphicGenericAdapter,
	#superclass : #SpAbstractMorphicAdapter,
	#category : #'Spec2-Deprecated-Adapters'
}

{ #category : #deprecation }
SpMorphicGenericAdapter class >> isDeprecated [
	"You should use #SpMorphPresenter instead of #asSpAdapter."

	^ true
]

{ #category : #'instance creation' }
SpMorphicGenericAdapter class >> morph: morph [

	^ self new
		morph: morph;
		yourself
]

{ #category : #accessing }
SpMorphicGenericAdapter >> adapter [

	^ self
]

{ #category : #api }
SpMorphicGenericAdapter >> buildWithSpec [

	^ self widget
]

{ #category : #api }
SpMorphicGenericAdapter >> buildWithSpec: aString [ 
	
	^ self buildWithSpec
]

{ #category : #accessing }
SpMorphicGenericAdapter >> morph [

	^ self widget
]

{ #category : #accessing }
SpMorphicGenericAdapter >> morph: anObject [

	widget := anObject
]
