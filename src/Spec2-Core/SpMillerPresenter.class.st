"
Basic API that a Miller Presenter should implement
"
Class {
	#name : #SpMillerPresenter,
	#superclass : #SpPresenter,
	#category : #'Spec2-Core-Miller'
}

{ #category : #initialization }
SpMillerPresenter >> whenActivatedDo: aBlock [

	self subclassResponsibility
]
