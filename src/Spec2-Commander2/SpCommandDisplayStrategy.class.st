"
I allow to configure how a spec group display itself when shown in UI.
"
Class {
	#name : #SpCommandDisplayStrategy,
	#superclass : #Object,
	#category : #'Spec2-Commander2-Core'
}

{ #category : #displaying }
SpCommandDisplayStrategy >> display: aCmSpecCommand in: aMenuPresenter do: aBlock [
	self subclassResponsibility
]
