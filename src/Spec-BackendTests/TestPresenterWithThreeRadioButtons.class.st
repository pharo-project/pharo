Class {
	#name : #TestPresenterWithThreeRadioButtons,
	#superclass : #ComposablePresenter,
	#instVars : [
		'radio1',
		'radio2',
		'radio3'
	],
	#category : #'Spec-BackendTests-TestClasses'
}

{ #category : #specs }
TestPresenterWithThreeRadioButtons class >> defaultSpec [

	^ SpecBoxLayout newVertical 
		add: #radio1;
		add: #radio2;
		add: #radio3;
		yourself
]

{ #category : #initialization }
TestPresenterWithThreeRadioButtons >> initializePresenter [ 

	radio1 state: false.
	radio2 state: true.
	radio3 state: false

]

{ #category : #initialization }
TestPresenterWithThreeRadioButtons >> initializeWidgets [
	
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
TestPresenterWithThreeRadioButtons >> radio1 [
	^ radio1
]

{ #category : #accessing }
TestPresenterWithThreeRadioButtons >> radio2 [
	^ radio2
]

{ #category : #accessing }
TestPresenterWithThreeRadioButtons >> radio3 [
	^ radio3
]
