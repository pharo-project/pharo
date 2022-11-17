Class {
	#name : #ScopeEditorPresenter,
	#superclass : #SpPresenter,
	#instVars : [
		'packagesTree',
		'scope'
	],
	#category : #'SystemCommands-RefactoringSupport-Scope'
}

{ #category : #'instance creation' }
ScopeEditorPresenter class >> defaultLayout [
	^ SpBoxLayout newTopToBottom
		  add: #packagesTree
		  expand: true
		  fill: true
		  padding: 0;
		  yourself
]

{ #category : #'instance creation' }
ScopeEditorPresenter class >> open [ 
	<script>
	self new open
]

{ #category : #'instance creation' }
ScopeEditorPresenter class >> title [
	^ 'Create/Edit a Scope'
]

{ #category : #initialization }
ScopeEditorPresenter >> initializePresenters [
	packagesTree := self newTreeTable.
	
	packagesTree
		addColumn: (SpCompositeTableColumn new
			title: 'Packages / Classes / Protocols';
			addColumn: ((SpImageTableColumn 
				evaluated: [ :anElement | self iconNamed: anElement class scopesIconName ])
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
