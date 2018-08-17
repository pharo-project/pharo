"
Utility class for connection of the SpecDemoFormModel with a fast table
"
Class {
	#name : #SpecDemoFormTableDataSource,
	#superclass : #FTSimpleDataSource,
	#instVars : [
		'model'
	],
	#category : #'Spec-Examples-Demo-Forms'
}

{ #category : #'instance creation' }
SpecDemoFormTableDataSource class >> for: aModel [
	^ self new 
		model: aModel;
		yourself
]

{ #category : #adding }
SpecDemoFormTableDataSource >> addColumnsTo: aTable [

	aTable
		addColumn: (FTColumn new 
			id: 'Name';
			width: 110;
			yourself);
		addColumn: (FTColumn new 
			id: 'Value';
			yourself)		


]

{ #category : #accessing }
SpecDemoFormTableDataSource >> cellColumn: column row: rowIndex [ 
	
	| item |
	
	item := self elementAt: rowIndex.

	column id = 'Name' ifTrue: [ ^ item key asStringMorph ].
	column id = 'Value' ifTrue: [ ^ item value asStringMorph ].



]

{ #category : #accessing }
SpecDemoFormTableDataSource >> model [
	^ model
]

{ #category : #accessing }
SpecDemoFormTableDataSource >> model: anObject [
	model := anObject.
	self refreshElements.
]

{ #category : #accessing }
SpecDemoFormTableDataSource >> refreshElements [

	self elements: { 
		'name' -> self model name.
		'surname' -> self model surname.
		'number 1' -> self model number1.
		'number 2' -> self model number2.
		'scale' -> self model scale.
		'password' -> self model password.
		'rememberMe' -> self model rememberMe.
		'date' -> self model date.
		'gender' -> self model gender.
		'items' -> self model selectedItems }
]
