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
		add: 'Double click to browse.'
			expand: false
			fill: false
			padding: 0;
		yourself
]

{ #category : #initialization }
SpDemoTreeTablePresenter >> initializePresenters [
	table1 := self newTreeTable.

	table1
		addColumn:
			(SpCompositeTableColumn new
				title: 'Classes';
				addColumn:
					((SpImageTableColumn evaluated: #systemIcon)
						width: 20;
						yourself);
				addColumn: (SpStringTableColumn evaluated: #name);
				yourself);
		addColumn: (SpStringTableColumn title: 'Number of subclasses' evaluated: [ :class | class subclasses size asString ]);
		addColumn: (SpStringTableColumn title: 'Number of methods' evaluated: [ :class | class methods size asString ]);
		roots: {Object};
		children: [ :aClass | aClass subclasses ];
		beResizable;
		contextMenu: self menu;
		activateOnDoubleClick;
		whenActivatedDo: [ :selection | selection selectedItem browse ]
]

{ #category : #accessing }
SpDemoTreeTablePresenter >> menu [
	^ self newMenu
		title: 'Context menu example';
		addGroup: [ :aGroup | 
			aGroup
				addItem: [ :anItem | 
					anItem
						name: 'Print in transcript';
						shortcut: $p meta;
						description: 'Print the selected class on Transcript';
						action: [ table1 selectedItem traceCr ] ];
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
												action: [ table1 selectedItem methods size traceCr ] ];
										addItem: [ :item | 
											item
												name: 'Print number of variables';
												action: [ table1 selectedItem instanceVariables size traceCr ] ] ]) ] ];
		addGroup: [ :aGroup | 
			aGroup
				addItem: [ :anItem | 
					anItem
						name: 'Item showing only if item a odd number of letters';
						visibleIf: [ table1 selectedItem printString size odd ];
						action: [ table1 selectedItem traceCr ] ];
				addItem: [ :anItem | 
					anItem
						name: 'Disabled in second group';
						disable;
						action: [ table1 selectedItem traceCr ] ];
				addItem: [ :anItem | 
					anItem
						name: 'Disabled if class begins with A';
						enabled: [ table1 selectedItem printString first ~= $A ];
						action: [ table1 selectedItem traceCr ] ] ]
]
