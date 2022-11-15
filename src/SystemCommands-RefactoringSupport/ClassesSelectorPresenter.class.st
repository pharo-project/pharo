Class {
	#name : #ClassesSelectorPresenter,
	#superclass : #SpPresenter,
	#instVars : [
		'packagesTree'
	],
	#category : #'SystemCommands-RefactoringSupport-Scope'
}

{ #category : #'instance creation' }
ClassesSelectorPresenter class >> defaultLayout [
	^ SpBoxLayout newTopToBottom
		  add: #packagesTree
		  expand: true
		  fill: true
		  padding: 0;
		  yourself
]

{ #category : #'instance creation' }
ClassesSelectorPresenter class >> open [ 
	<script>
	self new open
]

{ #category : #initialization }
ClassesSelectorPresenter >> initializePresenters [
	packagesTree := self newTreeTable.
	
	packagesTree
		addColumn: (SpCompositeTableColumn new
			title: 'Packages / Classes / Protocols';
			addColumn: ((SpImageTableColumn 
				evaluated: [ :aClass | self iconNamed: aClass systemIconName ])
				width: 200;
				yourself);
			addColumn: (SpStringTableColumn evaluated: #name);
			yourself);
		addColumn: (SpStringTableColumn 
			title: 'Number of subelements' 
			evaluated: [ :element | element subelements size asString ]);
		"addColumn: (SpStringTableColumn 
			title: 'Number of methods' 
			evaluated: [ :class | class methods size asString ]);"
		roots: Smalltalk packages;
		children: [ :aClass | aClass subelements ];
		beResizable;
		"contextMenu: self menu;"
		activateOnDoubleClick;
		whenActivatedDo: [ :selection | selection selectedItem browse ]
]
