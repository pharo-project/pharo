"
I am used to compute a MenuGroupPresenter.
There is not Morphic representation of a MenuGroup, that is why I do not have a coresponding morph.
"
Class {
	#name : #MorphicMenuGroupAdapter,
	#superclass : #AbstractMorphicAdapter,
	#category : #'Spec-MorphicAdapters-Base'
}

{ #category : #private }
MorphicMenuGroupAdapter >> buildWidget [

	^ self menuItems collect: [ :each | each buildWithSpec ]
]

{ #category : #protocol }
MorphicMenuGroupAdapter >> menuItems [
	
	^ self model menuItems
]
