Class {
	#name : #SpecGridExample,
	#superclass : #ComposablePresenter,
	#instVars : [
		'label1',
		'input1',
		'label2',
		'input2'
	],
	#category : #'Spec-Examples'
}

{ #category : #specs }
SpecGridExample class >> diagonalSpec [ 
	<spec>
	
	^ SpecGridLayout new 
		add: #label1 at: 1@1;
		add: #input1 at: 2@2;
		add: #label2 at: 3@3;
		add: #input2 at: 4@4;
		yourself
]

{ #category : #examples }
SpecGridExample class >> example1 [ 

	^ self new openWithSpec: #simpleSpec
]

{ #category : #examples }
SpecGridExample class >> example2 [

	^ self new openWithSpec: #diagonalSpec
]

{ #category : #specs }
SpecGridExample class >> simpleSpec [ 
	<spec: #default>
	
	^ SpecGridLayout new 
		add: #label1 at: 1@1;
		add: #input1 at: 2@1;
		add: #label2 at: 1@2;
		add: #input2 at: 2@2;
		yourself
]

{ #category : #initialization }
SpecGridExample >> initializeWidgets [
	
	label1 := self newLabel label: 'Label 1'.
	input1 := self newTextInput text: 'Input 1'.
	label2 := self newLabel label: 'Label 2'.
	input2 := self newTextInput text: 'Input 2'
]
