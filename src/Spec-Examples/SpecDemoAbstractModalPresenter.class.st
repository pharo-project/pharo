"
Description
--------------------

I am an abstract class for example modals.
"
Class {
	#name : #SpecDemoAbstractModalPresenter,
	#superclass : #ComposablePresenter,
	#instVars : [
		'explanation'
	],
	#category : #'Spec-Examples-Demo-Modals'
}

{ #category : #specs }
SpecDemoAbstractModalPresenter class >> defaultSpec [
	^ SpecLayout composed
		add: #explanation;
		yourself
]

{ #category : #accessing }
SpecDemoAbstractModalPresenter >> content [
	^ self subclassResponsibility
]

{ #category : #accessing }
SpecDemoAbstractModalPresenter >> explanation [
	^ explanation
]

{ #category : #initialization }
SpecDemoAbstractModalPresenter >> initializeWidgets [
	explanation := self newText.
	
	explanation text: self content
]
