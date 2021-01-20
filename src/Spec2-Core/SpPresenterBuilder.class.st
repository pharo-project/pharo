"
A simple presenter builder class that uses `SpTPresenterBuilder`. 
It allows users to compose presenters without hardcoding class name.
"
Class {
	#name : #SpPresenterBuilder,
	#superclass : #Object,
	#traits : 'SpTPresenterBuilder',
	#category : #'Spec2-Core-Base'
}

{ #category : #'instance creation' }
SpPresenterBuilder >> instantiate: aPresenterClass [
	"Instantiate a SpPresenter subclass and set its instance owner"
	^ aPresenterClass new
]

{ #category : #'instance creation' }
SpPresenterBuilder >> instantiate: aPresenterClass on: aModel [
	"Instantiate a SpPresenter subclass and set its instance owner and model"
	^ aPresenterClass on: aModel
]
