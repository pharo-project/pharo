"
I'm a spec adapter for a morph
"
Class {
	#name : #SpMorphicMorphAdapter,
	#superclass : #SpAbstractMorphicAdapter,
	#category : #'Spec2-Adapters-Morphic-Base'
}

{ #category : #factory }
SpMorphicMorphAdapter >> buildWidget [
	| newWidget |

	newWidget := self presenter morph 
		ifNil: [ Morph new color: Color transparent ].
	newWidget presenter: self presenter.

	self presenter whenMorphChangedDo: [ :aMorph | 
		self updateMorph: aMorph ].
	
	^ newWidget
]

{ #category : #factory }
SpMorphicMorphAdapter >> updateMorph: aMorph [

	self applyStyle: aMorph.
	aMorph presenter: self presenter.
	widget owner ifNotNil: [ :ownerMorph | 
		ownerMorph 
			replaceSubmorph: widget
			by: aMorph ].
	widget := aMorph
]
