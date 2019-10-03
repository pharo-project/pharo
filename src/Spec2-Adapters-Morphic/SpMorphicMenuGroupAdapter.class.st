"
I am used to compute a MenuGroupPresenter.
There is not Morphic represenation of a MenuGroup, that is why I do not have a coresponding morph.
"
Class {
	#name : #SpMorphicMenuGroupAdapter,
	#superclass : #SpAbstractMorphicAdapter,
	#category : #'Spec2-Adapters-Morphic-Base'
}

{ #category : #initialization }
SpMorphicMenuGroupAdapter >> addKeyBindingsTo: aMorph [
	"This does not has keybindings"
]

{ #category : #private }
SpMorphicMenuGroupAdapter >> applyVisibility [

	"Do nothing. Morphs implement visible but the widget of this adapter is an ordered collection."
]

{ #category : #private }
SpMorphicMenuGroupAdapter >> buildWidget [
	^ self menuItems select: #isVisible thenCollect: #buildWithSpec
]

{ #category : #protocol }
SpMorphicMenuGroupAdapter >> menuItems [
	
	^ self model menuItems
]
