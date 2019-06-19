"
This demo shows differenty styles of check boxes.
"
Class {
	#name : #SpecDemoCheckboxesPresenter,
	#superclass : #ComposablePresenter,
	#instVars : [
		'checkboxNormal',
		'checkboxDisabled',
		'checkboxWithColor',
		'checkboxWithHelp',
		'checkboxWithLabelOnLeft',
		'checkboxExample'
	],
	#category : #'Spec-Examples-Demo-Checkboxes'
}

{ #category : #specs }
SpecDemoCheckboxesPresenter class >> defaultSpec [
	^ SpecBoxLayout newHorizontal
		add:
			(SpecBoxLayout newVertical
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
SpecDemoCheckboxesPresenter >> checkboxDisabled [
	^ checkboxDisabled
]

{ #category : #accessing }
SpecDemoCheckboxesPresenter >> checkboxDisabled: anObject [
	checkboxDisabled := anObject
]

{ #category : #accessing }
SpecDemoCheckboxesPresenter >> checkboxNormal [
	^ checkboxNormal
]

{ #category : #accessing }
SpecDemoCheckboxesPresenter >> checkboxNormal: anObject [
	checkboxNormal := anObject
]

{ #category : #accessing }
SpecDemoCheckboxesPresenter >> checkboxWithColor [
	^ checkboxWithColor
]

{ #category : #accessing }
SpecDemoCheckboxesPresenter >> checkboxWithColor: anObject [
	checkboxWithColor := anObject
]

{ #category : #accessing }
SpecDemoCheckboxesPresenter >> checkboxWithHelp [
	^ checkboxWithHelp
]

{ #category : #accessing }
SpecDemoCheckboxesPresenter >> checkboxWithHelp: anObject [
	checkboxWithHelp := anObject
]

{ #category : #accessing }
SpecDemoCheckboxesPresenter >> checkboxWithLabelOnLeft [
	^ checkboxWithLabelOnLeft
]

{ #category : #accessing }
SpecDemoCheckboxesPresenter >> checkboxWithLabelOnLeft: anObject [
	checkboxWithLabelOnLeft := anObject
]

{ #category : #initialization }
SpecDemoCheckboxesPresenter >> initializeWidgets [

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
	checkboxExample := self instantiate: CheckBoxExample.
		
	self whenBuiltDo: [ 
		checkboxWithLabelOnLeft labelOnLeft.].		
																						
]
