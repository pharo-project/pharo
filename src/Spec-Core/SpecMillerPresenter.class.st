"
Basic API that a Miller Presenter should implement
"
Class {
	#name : #SpecMillerPresenter,
	#superclass : #ComposablePresenter,
	#category : #'Spec-Core-Miller'
}

{ #category : #initialization }
SpecMillerPresenter >> whenActivatedDo: aBlock [

	self subclassResponsibility
]
