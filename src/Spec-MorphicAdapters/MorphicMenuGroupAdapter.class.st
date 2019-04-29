"
I am used to compute a MenuGroupPresenter.
There is not Morphic represenation of a MenuGroup, that is why I do not have a coresponding morph.
"
Class {
	#name : #MorphicMenuGroupAdapter,
	#superclass : #AbstractMorphicAdapter,
	#category : #'Spec-MorphicAdapters-Base'
}

{ #category : #private }
MorphicMenuGroupAdapter >> applyVisibility [

	"Do nothing. Morphs implement visible but the widget of this adapter is an ordered collection."
]

{ #category : #private }
MorphicMenuGroupAdapter >> buildWidget [
	^ self menuItems select: #isVisible thenCollect: #buildWithSpec
]

{ #category : #protocol }
MorphicMenuGroupAdapter >> menuItems [
	
	^ self model menuItems
]
