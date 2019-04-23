"
Description
--------------------

I show demos of tree tables
"
Class {
	#name : #SpecDemoTreeTablePresenter,
	#superclass : #ComposablePresenter,
	#instVars : [
		'table1'
	],
	#category : #'Spec-Examples-Demo-Tables'
}

{ #category : #specs }
SpecDemoTreeTablePresenter class >> defaultSpec [

	^ SpecBoxLayout newVertical
		add: #table1;
		yourself
]

{ #category : #initialization }
SpecDemoTreeTablePresenter >> initializeWidgets [
	table1 := self newTreeTable.

	table1
		addColumn:
			(CompositeTableColumn new
				title: 'Classes';
				addColumn:
					(ImageTableColumn new
						width: 20;
						evaluated: #systemIcon;
						yourself);
				addColumn:
					(StringTableColumn new
						evaluated: #name;
						yourself);
				yourself);
		addColumn: (StringTableColumn title: 'Number of subclasses' evaluated: [ :class | class subclasses size asString ]);
		addColumn: (StringTableColumn title: 'Number of methods' evaluated: [ :class | class methods size asString ]);
		roots: {Object};
		children: [ :aClass | aClass subclasses ];
		beResizable
]
