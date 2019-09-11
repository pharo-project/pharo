"
I am a visitor that builds a Spec MenuBarPresenter from a commands and commands group.
"
Class {
	#name : #SpMenuBarPresenterBuilder,
	#superclass : #SpMenuPresenterBuilder,
	#category : #'Spec2-Commander2-Visitors'
}

{ #category : #accessing }
SpMenuBarPresenterBuilder class >> menuPresenterClass [
	^ SpMenuBarPresenter
]
