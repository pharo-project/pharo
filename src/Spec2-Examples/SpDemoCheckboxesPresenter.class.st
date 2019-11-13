"
This demo shows differenty styles of check boxes.
"
Class {
	#name : #SpDemoCheckboxesPresenter,
	#superclass : #SpPresenter,
	#instVars : [
		'checkboxNormal',
		'checkboxDisabled',
		'checkboxWithColor',
		'checkboxWithHelp',
		'checkboxWithLabelOnLeft',
		'checkboxExample'
	],
	#category : #'Spec2-Examples-Demo-Checkboxes'
}

{ #category : #specs }
SpDemoCheckboxesPresenter class >> defaultSpec [
	^ SpBoxLayout newHorizontal
		add:
			(SpBoxLayout newVertical
				add: #checkboxNormal;
				add: #checkboxDisabled;
				add: #checkboxWithColor;
				add: #checkboxWithHelp;
				add: #checkboxWithLabelOnLeft;
				yourself);
		add: #checkboxExample;
		yourself
]

{ #category : #accessing }
SpDemoCheckboxesPresenter >> checkboxDisabled [
	^ checkboxDisabled
]

{ #category : #accessing }
SpDemoCheckboxesPresenter >> checkboxDisabled: anObject [
	checkboxDisabled := anObject
]

{ #category : #accessing }
SpDemoCheckboxesPresenter >> checkboxNormal [
	^ checkboxNormal
]

{ #category : #accessing }
SpDemoCheckboxesPresenter >> checkboxNormal: anObject [
	checkboxNormal := anObject
]

{ #category : #accessing }
SpDemoCheckboxesPresenter >> checkboxWithColor [
	^ checkboxWithColor
]

{ #category : #accessing }
SpDemoCheckboxesPresenter >> checkboxWithColor: anObject [
	checkboxWithColor := anObject
]

{ #category : #accessing }
SpDemoCheckboxesPresenter >> checkboxWithHelp [
	^ checkboxWithHelp
]

{ #category : #accessing }
SpDemoCheckboxesPresenter >> checkboxWithHelp: anObject [
	checkboxWithHelp := anObject
]

{ #category : #accessing }
SpDemoCheckboxesPresenter >> checkboxWithLabelOnLeft [
	^ checkboxWithLabelOnLeft
]

{ #category : #accessing }
SpDemoCheckboxesPresenter >> checkboxWithLabelOnLeft: anObject [
	checkboxWithLabelOnLeft := anObject
]

{ #category : #initialization }
SpDemoCheckboxesPresenter >> initializePresenters [

	checkboxNormal  := self newCheckBox
		label: 'normal'.
	checkboxDisabled := self newCheckBox 
		label: 'disabled';
		disable;
		yourself.
	checkboxWithColor := self newCheckBox 
		label: 'red';
		color: Color red;
		yourself.
	checkboxWithHelp := self newCheckBox 
		label: 'with help';
		help: 'this is checkbox with help';
		yourself.
	checkboxWithLabelOnLeft := self newCheckBox 
		label: 'left';
		yourself.
	checkboxExample := self instantiate: SpCheckBoxExample.
		
	self whenBuiltDo: [ 
		checkboxWithLabelOnLeft labelOnLeft.].		
																						
]
