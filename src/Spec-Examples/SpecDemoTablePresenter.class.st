"
Description
--------------------

I show demos of tables
"
Class {
	#name : #SpecDemoTablePresenter,
	#superclass : #ComposablePresenter,
	#instVars : [
		'table1',
		'label',
		'selectedClasses'
	],
	#category : #'Spec-Examples-Demo-Tables'
}

{ #category : #specs }
SpecDemoTablePresenter class >> defaultSpec [
	^ SpecBoxLayout newVertical
		add: #table1;
		add: #label withConstraints: [ :c | c height: self labelHeight ];
		yourself
]

{ #category : #initialization }
SpecDemoTablePresenter >> initialize [
	selectedClasses := OrderedCollection with: ASTCache with: ASTEvaluationTest with: Abort.
	super initialize
]

{ #category : #initialization }
SpecDemoTablePresenter >> initializeWidgets [
	table1 := self newTable.
	label := self newLabel.

	self resetLabel.

	table1
		addColumn:
			((CheckBoxTableColumn title: ' ' evaluated: [ :class | selectedClasses includes: class ])
				onActivation: [ :class | 
					selectedClasses add: class.
					self resetLabel ];
				onDesactivation: [ :class | 
					selectedClasses remove: class.
					self resetLabel ];
				width: 20;
				yourself);
		addColumn:
			((ImageTableColumn title: 'Icon' evaluated: #systemIcon)
				width: 50;
				yourself);
		addColumn: (StringTableColumn title: 'Name' evaluated: #name);
		addColumn: (StringTableColumn title: 'Number of subclasses' evaluated: [ :class | class subclasses size asString ]);
		items: self class environment allClasses;
		beResizable
]

{ #category : #initialization }
SpecDemoTablePresenter >> resetLabel [
	label label: (selectedClasses ifEmpty: [ 'No class checked' ] ifNotEmpty: [ (selectedClasses collect: #printString) joinUsing: ' - '])
]
