"
Description
--------------------

I am an abstract class for example modals.
"
Class {
	#name : #SpDemoAbstractModalPresenter,
	#superclass : #SpPresenter,
	#instVars : [
		'explanation'
	],
	#category : #'Spec2-Examples-Demo-Modals'
}

{ #category : #specs }
SpDemoAbstractModalPresenter class >> defaultSpec [
	^ SpBoxLayout newVertical
		add: #explanation;
		yourself
]

{ #category : #accessing }
SpDemoAbstractModalPresenter >> content [
	^ self subclassResponsibility
]

{ #category : #accessing }
SpDemoAbstractModalPresenter >> explanation [
	^ explanation
]

{ #category : #initialization }
SpDemoAbstractModalPresenter >> initializePresenters [
	explanation := self newText.
	
	explanation text: self content
]
