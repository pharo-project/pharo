"
This demo shows, how to create a simple form with basic elements and how to connect this form to a model. 

In this case, the model is represented by instances of the class SpecDemoFormModel created in the #initialize. It is a simple structure-like class with some default values. The model is not held directly but using a value holder named announcingObject (see ComposablePresenterWithModel>>#model:). The encapsulating value holder would not be needed if our model would be a subclass of ""Model"" (instances of Model provide an announcer by default).

This component has two main subcomponents. The form and a table that shows the current state of the model.

The form is a standalone Spec presenter, an instance of SpecDemoStandaloneFormPresenter. It works with two instances of the model, ""workingModel"" which holds a working copy of the model and a model shared with its parent. When the form is restored, the working copy of the model is thrown away and replaced with a new copy of the model. When the form is submitted, the new model is stored (into the value holder and its change is propagated so the table showing the state of the model is refreshed).

The data exchange between the model and form elements is done in these methods:
SpecDemoStandaloneFormPresenter>>#fillFormWithWorkingModel
SpecDemoStandaloneFormPresenter>>#fillModelWithFormContent

The two parts of this window (form and table) should be rendered in boxes with an outline. Spec in Morphic does not have such functionality now. SpecDemoLabeledContainer is used now.

Text input for number 1 is limited directly on the level of user input. See SpecDemoStandaloneFormPresenter>>#initializePresenter. 

For displaying of the model state, the FastTable Morph is used. Currently, Spec does not provide a presenter for such tables. The class SpecDemoFormTableDataSource is used for interaction between the table and model. 




"
Class {
	#name : #SpecDemoFormPresenter,
	#superclass : #ComposablePresenterWithModel,
	#instVars : [
		'form',
		'resultPane',
		'table'
	],
	#category : #'Spec-Examples-Demo-Forms'
}

{ #category : #specs }
SpecDemoFormPresenter class >> defaultSpec [

	<spec: #default>

	| fontWidth formWidth |
	fontWidth := (StandardFonts defaultFont widthOfString: 'M').
	formWidth := fontWidth * 35.
	
	^ SpecLayout composed
		newRow: [ :row | 
			row add: #form right: formWidth.
			row add: #resultPane];
		yourself.	
]

{ #category : #accessing }
SpecDemoFormPresenter >> form [
	^ form
]

{ #category : #accessing }
SpecDemoFormPresenter >> form: anObject [
	form := anObject
]

{ #category : #accessing }
SpecDemoFormPresenter >> initialize [

	self model: SpecDemoFormModel new.
	super initialize.
]

{ #category : #accessing }
SpecDemoFormPresenter >> initializeWidgets [

	form := (self instantiate: SpecDemoLabeledContainer)
		label: 'form';
		subwidget: (self instantiate:  SpecDemoStandaloneFormPresenter on: self announcingObject).
	resultPane := (self instantiate: SpecDemoLabeledContainer)
		label: 'result';
		subwidget: (table := self newDataTable).

	self focusOrder 
		add: form;
		add: table.
		
]

{ #category : #accessing }
SpecDemoFormPresenter >> modelChanged [

	table widget dataSource model: self model.
	table widget refresh.
]

{ #category : #accessing }
SpecDemoFormPresenter >> newDataTable [

	| aTable tableDataSource |
	aTable := FTTableMorph new
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		asSpecAdapter.

	tableDataSource := SpecDemoFormTableDataSource for: self model.
		
	aTable widget 
		beResizable.
	tableDataSource addColumnsTo: aTable widget.
	aTable widget dataSource: tableDataSource.

	^ aTable
		
]

{ #category : #accessing }
SpecDemoFormPresenter >> resultPane [
	^ resultPane
]

{ #category : #accessing }
SpecDemoFormPresenter >> resultPane: anObject [
	resultPane := anObject
]

{ #category : #accessing }
SpecDemoFormPresenter >> table [
	^ table
]

{ #category : #accessing }
SpecDemoFormPresenter >> table: anObject [
	table := anObject
]
