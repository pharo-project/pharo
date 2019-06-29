"
Description
--------------------

I show demos of tree tables
"
Class {
	#name : #SpDemoTreeTablePresenter,
	#superclass : #SpPresenter,
	#instVars : [
		'table1'
	],
	#category : #'Spec2-Examples-Demo-Tables'
}

{ #category : #specs }
SpDemoTreeTablePresenter class >> defaultSpec [

	^ SpBoxLayout newVertical
		add: #table1;
		yourself
]

{ #category : #initialization }
SpDemoTreeTablePresenter >> initializeWidgets [
	table1 := self newTreeTable.

	table1
		addColumn:
			(SpCompositeTableColumn new
				title: 'Classes';
				addColumn:
					(SpImageTableColumn new
						width: 20;
						evaluated: #systemIcon;
						yourself);
				addColumn:
					(SpStringTableColumn new
						evaluated: #name;
						yourself);
				yourself);
		addColumn: (SpStringTableColumn title: 'Number of subclasses' evaluated: [ :class | class subclasses size asString ]);
		addColumn: (SpStringTableColumn title: 'Number of methods' evaluated: [ :class | class methods size asString ]);
		roots: {Object};
		children: [ :aClass | aClass subclasses ];
		beResizable;
		contextMenu: self menu
]

{ #category : #accessing }
SpDemoTreeTablePresenter >> menu [
	^ self newMenu
		addTitle: 'Context menu example';
		addGroup: [ :aGroup | 
			aGroup
				addItem: [ :anItem | 
					anItem
						name: 'Print in transcript';
						shortcut: $p meta;
						description: 'Print the selected class on Transcript';
						action: [ table1 selectedItem logCr ] ];
				addItem: [ :anItem | 
					anItem
						name: 'Browse';
						iconName: #open;
						action: [ table1 selectedItem browse ] ];
				addItem: [ :anItem | 
					anItem
						name: 'Print infos';
						subMenu:
							(self newMenu
								addGroup: [ :aSubGroup | 
									aSubGroup
										addItem: [ :item | 
											item
												name: 'Print number of methods';
												action: [ table1 selectedItem methods size logCr ] ];
										addItem: [ :item | 
											item
												name: 'Print number of variables';
												action: [ table1 selectedItem instanceVariables size logCr ] ] ]) ] ];
		addGroup: [ :aGroup | 
			aGroup
				addItem: [ :anItem | 
					anItem
						name: 'Item showing only if item a odd number of letters';
						visibleIf: [ table1 selectedItem printString size odd ];
						action: [ table1 selectedItem logCr ] ];
				addItem: [ :anItem | 
					anItem
						name: 'Disabled in second group';
						disable;
						action: [ table1 selectedItem logCr ] ];
				addItem: [ :anItem | 
					anItem
						name: 'Disabled if class begins with A';
						enabled: [ table1 selectedItem printString first ~= $A ];
						action: [ table1 selectedItem logCr ] ] ]
]
