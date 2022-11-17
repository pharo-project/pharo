Class {
	#name : #ScopesPresenter,
	#superclass : #SpPresenter,
	#instVars : [
		'scopesTree',
		'toolbar'
	],
	#category : #'SystemCommands-RefactoringSupport-Scope'
}

{ #category : #layout }
ScopesPresenter class >> defaultLayout [
	^ SpBoxLayout newTopToBottom
		  add: #toolbar
		  expand: false
		  fill: false
		  padding: 0;
		  add: #scopesTree;
		  yourself
]

{ #category : #'world menu' }
ScopesPresenter class >> menuCommandOn: aBuilder [
    <worldMenu>
    (aBuilder item: #'Scopes Editor')
                action: [self open];
                parent: #Refactoring;
                help: 'Editor for custom Scopes (refactoring environments)';
                order: 10;
					iconName: #smallPaint
]

{ #category : #layout }
ScopesPresenter class >> open [

	<example>
	^ self new open
]

{ #category : #layout }
ScopesPresenter class >> title [
	^ 'Scope Browser'
]

{ #category : #initialization }
ScopesPresenter >> initializePresenters [

	scopesTree := self newTreeTable.
	toolbar := self newToolbar.
	self toolbarButtons do: [ :button | toolbar addItem: button ]
]

{ #category : #actions }
ScopesPresenter >> showScopeEditor [
	ScopeEditorPresenter new open
]

{ #category : #initialization }
ScopesPresenter >> toolbarButtons [

	| buttons |
	buttons := OrderedCollection new.
	buttons add: (self newToolbarButton
			 icon: (self iconNamed: #add);
			 help: 'Add a new scope';
			 action: [ self showScopeEditor ];
			 yourself).
	buttons add: (self newToolbarButton
			 icon: (self iconNamed: #glamorousTrash);
			 help: 'Delete selected scope';
			 action: [ self deleteSelectedScope ];
			 disable;
			 yourself).

	^ buttons
]
