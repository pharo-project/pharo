"
I allow to configure how a spec command group display when shown in UI.
"
Class {
	#name : #SpCommandGroupDisplayStrategy,
	#superclass : #Object,
	#category : #'Spec2-Commander2-Core'
}

{ #category : #displaying }
SpCommandGroupDisplayStrategy >> display: aCmSpecCommandGroup in: aMenuPresenter do: aBlock [
	self subclassResponsibility
]
