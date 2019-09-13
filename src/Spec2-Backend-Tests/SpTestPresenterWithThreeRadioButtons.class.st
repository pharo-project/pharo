Class {
	#name : #SpTestPresenterWithThreeRadioButtons,
	#superclass : #SpPresenter,
	#instVars : [
		'radio1',
		'radio2',
		'radio3'
	],
	#category : #'Spec2-Backend-Tests-TestClasses'
}

{ #category : #specs }
SpTestPresenterWithThreeRadioButtons class >> defaultSpec [

	^ SpBoxLayout newVertical 
		add: #radio1;
		add: #radio2;
		add: #radio3;
		yourself
]

{ #category : #initialization }
SpTestPresenterWithThreeRadioButtons >> connectPresenters [ 

	radio1 state: false.
	radio2 state: true.
	radio3 state: false

]

{ #category : #initialization }
SpTestPresenterWithThreeRadioButtons >> initializePresenters [
	
	radio1 := self newRadioButton
					label: 'radio1';
					yourself. 
	radio2 := self newRadioButton
					label: 'radio2';
					yourself. 

	radio3 := self newRadioButton
					label: 'radio3';
					yourself. 
	
	radio1 associatedRadioButtons: {radio2. radio3}.

]

{ #category : #accessing }
SpTestPresenterWithThreeRadioButtons >> radio1 [
	^ radio1
]

{ #category : #accessing }
SpTestPresenterWithThreeRadioButtons >> radio2 [
	^ radio2
]

{ #category : #accessing }
SpTestPresenterWithThreeRadioButtons >> radio3 [
	^ radio3
]
