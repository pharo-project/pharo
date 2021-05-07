Class {
	#name : #RBRBEnvironmentsPresenter,
	#superclass : #SpPresenter,
	#instVars : [
		'scopeList'
	],
	#category : #'SystemCommands-RefactoringSupport-Scope'
}

{ #category : #specs }
RBRBEnvironmentsPresenter class >> defaultSpec [
	^ SpBoxLayout newTopToBottom
		add:
			(SpBoxLayout newLeftToRight
				add: #scopeList;
				yourself);
		yourself
]

{ #category : #specs }
RBRBEnvironmentsPresenter class >> example [
	<script>
	(self new) openWithSpec
]

{ #category : #accessing }
RBRBEnvironmentsPresenter class >> title [ 
	^ 'Scope options'
]

{ #category : #accessing }
RBRBEnvironmentsPresenter >> andWithScope: anItem [ 
	self performOperation: #& with: anItem
]

{ #category : #accessing }
RBRBEnvironmentsPresenter >> argumentsListMenu [
	^ self newMenu
		addGroup: [ :aGroup | 
			aGroup
				addItem: [ :anItem | 
					anItem
						name: 'AND';
						action: [ self andWithScope: scopeList selectedItem ] ];
				addItem: [ :anItem | 
					anItem
						name: 'OR';
						action: [ self orWithScope: scopeList selectedItem ] ];
				addItem: [ :anItem | 
					anItem
						name: 'Inspect';
						action: [ self inspect: scopeList selectedItem ] ];
				addItem: [ :anItem | 
					anItem
						name: 'Edit';
						action: [ self edit: scopeList selectedItem ] ];
				addItem: [ :anItem | 
					anItem
						name: 'Remove';
						action: [ self remove: scopeList selectedItem ] ]
				 ]
]

{ #category : #actions }
RBRBEnvironmentsPresenter >> edit: anItem [
	[ anItem acceptVisitor: RBEnvironmentVisitor new ]
	on: Error 
	do: [ :e | self halt. self inform:  'Somethig was wrong']
	
]

{ #category : #accessing }
RBRBEnvironmentsPresenter >> initializePresenter [

	self updateList.
	scopeList contextMenu: self argumentsListMenu.

]

{ #category : #accessing }
RBRBEnvironmentsPresenter >> initializeWidgets [
	scopeList := self newList.
]

{ #category : #actions }
RBRBEnvironmentsPresenter >> inspect: anItem [
	anItem inspect
]

{ #category : #accessing }
RBRBEnvironmentsPresenter >> orWithScope: anItem [ 
	self performOperation: #| with: anItem
]

{ #category : #accessing }
RBRBEnvironmentsPresenter >> performOperation: aSymbol with: anItem [
	| anEnvironment newScope |
	[ anEnvironment := self selectAnotherScope.
	newScope := anItem perform: aSymbol with: anEnvironment.
	(self confirm: 'Do you want store this result in the list?') 
		ifTrue: [ |key|
			key := UIManager default
				request: 'Enter a keyword name for new scope'
				initialAnswer: anItem label , ' ', aSymbol , ' ', anEnvironment label.
			key isEmptyOrNil ifTrue: [ Error signal ].
			RefactoringSettings addScope: newScope withKey: key.
			self updateList.
		] ifFalse: [ newScope inspect ].
	] 
	on: Error 
	do: [ self inform: 'Something was wrong']
]

{ #category : #actions }
RBRBEnvironmentsPresenter >> remove: anItem [
	RefactoringSettings removeScopeNamed: anItem label.
	self updateList 
]

{ #category : #actions }
RBRBEnvironmentsPresenter >> rename: anItem [
	[	|key|
		key := UIManager default
				request: 'Enter a keyword name for new scope'
				initialAnswer: anItem label.
		(key isEmptyOrNil or: [ RefactoringSettings invalidNames includes: key ]) ifTrue: [ Error signal ].
		anItem label: key.	
		self updateList.
	]
	on: Error 
	do: [ self inform:  'Somethig was wrong']
	
]

{ #category : #actions }
RBRBEnvironmentsPresenter >> selectAnotherScope [ 
	| anotherScope |
	anotherScope := UIManager default
			chooseOrRequestFrom: RefactoringSettings scopeList values
			title: 'Select another scope (RBEnvironment)'.
	^ anotherScope 
]

{ #category : #accessing }
RBRBEnvironmentsPresenter >> updateList [ 
	scopeList items: RefactoringSettings scopeList
]
