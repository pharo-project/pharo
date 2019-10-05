Class {
	#name : #SpTableWithFilter,
	#superclass : #SpPresenter,
	#instVars : [
		'table'
	],
	#category : #'Spec2-Examples-Standalone'
}

{ #category : #specs }
SpTableWithFilter class >> defaultSpec [

	^ SpBoxLayout newVertical
		add: #table;
		yourself
]

{ #category : #initialization }
SpTableWithFilter >> initializePresenters [
	<script: 'self new openWithSpec'>

	table := self newTable
		addColumn: (SpCompositeTableColumn new
			title: 'Classes';
			addColumn: ((SpImageTableColumn evaluated: #systemIcon) beNotExpandable);
			addColumn: (SpStringTableColumn evaluated: #name);
			yourself);
		items: Smalltalk allClassesAndTraits;
		itemFilter: [ :each :pattern | 
			each crTrace.
			each asLowercase includesSubstring: pattern asLowercase ];
		yourself
]
