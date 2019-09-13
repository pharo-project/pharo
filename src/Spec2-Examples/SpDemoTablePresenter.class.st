"
Description
--------------------

I show demos of tables
"
Class {
	#name : #SpDemoTablePresenter,
	#superclass : #SpPresenter,
	#instVars : [
		'table1',
		'label',
		'selectedClasses'
	],
	#category : #'Spec2-Examples-Demo-Tables'
}

{ #category : #specs }
SpDemoTablePresenter class >> defaultSpec [
	^ SpBoxLayout newVertical
		add: #table1;
		add: #label withConstraints: [ :c | c height: self labelHeight ];
		yourself
]

{ #category : #initialization }
SpDemoTablePresenter >> initialize [
	selectedClasses := OrderedCollection with: ASTCache with: ASTEvaluationTest with: Abort.
	super initialize
]

{ #category : #initialization }
SpDemoTablePresenter >> initializePresenters [
	table1 := self newTable.
	label := self newLabel.

	self resetLabel.

	table1
		addColumn:
			((SpCheckBoxTableColumn title: ' ' evaluated: [ :class | selectedClasses includes: class ])
				onActivation: [ :class | 
					selectedClasses add: class.
					self resetLabel ];
				onDesactivation: [ :class | 
					selectedClasses remove: class.
					self resetLabel ];
				width: 20;
				yourself);
		addColumn:
			((SpImageTableColumn title: 'Icon' evaluated: #systemIcon)
				width: 50;
				yourself);
		addColumn: (SpStringTableColumn title: 'Name' evaluated: #name);
		addColumn: (SpStringTableColumn title: 'Number of subclasses' evaluated: [ :class | class subclasses size asString ]);
		items: self class environment allClasses;
		beResizable
]

{ #category : #initialization }
SpDemoTablePresenter >> resetLabel [
	label label: (selectedClasses ifEmpty: [ 'No class checked' ] ifNotEmpty: [ (selectedClasses collect: #printString) joinUsing: ' - '])
]
