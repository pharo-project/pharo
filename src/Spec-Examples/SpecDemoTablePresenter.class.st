"
Description
--------------------

I show demos of tables
"
Class {
	#name : #SpecDemoTablePresenter,
	#superclass : #ComposablePresenter,
	#instVars : [
		'table1'
	],
	#category : #'Spec-Examples-Demo-Tables'
}

{ #category : #specs }
SpecDemoTablePresenter class >> defaultSpec [

	^ SpecBoxLayout newVertical
		add: #table1;
		yourself
]

{ #category : #initialization }
SpecDemoTablePresenter >> initializeWidgets [
	table1 := self newTable.

	table1
		addColumn:
			((ImageTableColumn title: 'Icon' evaluated: #systemIcon)
				width: 50;
				yourself);
		addColumn: (StringTableColumn title: 'Name' evaluated: #name);
		addColumn: (StringTableColumn title: 'Number of subclasses' evaluated: [ :class | class subclasses size asString ]);
		items: Smalltalk allClasses;
		beResizable
]
