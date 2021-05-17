Class {
	#name : #RBEnvironmentsPresenter,
	#superclass : #SpPresenter,
	#instVars : [
		'scopeList'
	],
	#category : #'SystemCommands-RefactoringSupport-Scope'
}

{ #category : #specs }
RBEnvironmentsPresenter class >> defaultSpec [
	^ SpBoxLayout newTopToBottom
		add:
			(SpBoxLayout newLeftToRight
				add: #scopeList;
				yourself);
		yourself
]

{ #category : #'world menu' }
RBEnvironmentsPresenter class >> menuCommandOn: aBuilder [
    <worldMenu>
    (aBuilder item: #'Scopes Editor')
                action: [self open];
                parent: #Refactoring;
                help: 'Editor for custom refactoring environments (scopes)';
                order: 10
]

{ #category : #specs }
RBEnvironmentsPresenter class >> open [
	<script>
	(self new) openWithSpec
]

{ #category : #accessing }
RBEnvironmentsPresenter class >> title [ 
	^ 'Scope options'
]

{ #category : #accessing }
RBEnvironmentsPresenter >> andWithScope: anItem [ 
	self performOperation: #& with: anItem
]

{ #category : #accessing }
RBEnvironmentsPresenter >> argumentsListMenu [
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
						visibleIf: [ (scopeList selectedItem isCompositeEnvironment not
							and: [ scopeList selectedItem label ~= 'current image' ])];
						action: [ self edit: scopeList selectedItem ] ];
				addItem: [ :anItem | 
					anItem
						name: 'Remove';
						visibleIf: [ scopeList selectedItem label ~= 'current image' ];
						action: [ self remove: scopeList selectedItem ] ]
				 ]
]

{ #category : #actions }
RBEnvironmentsPresenter >> edit: anItem [
	[ anItem acceptVisitor: RBEnvironmentVisitor new ]
	on: Error 
	do: [ :e | self inform:  'Somethig was wrong']
	
]

{ #category : #accessing }
RBEnvironmentsPresenter >> initializePresenter [

	scopeList addColumn: (SpStringTableColumn title: 'Environment' evaluated: #shortDescription);
		roots: RefactoringSettings scopeList;
		children: [ :aClass | RefactoringSettings scopeList 
			select: [ :cls | aClass isCompositeEnvironment ifTrue: 
				[ aClass environment == cls or: [ aClass otherEnvironment == cls ] ] 
			ifFalse: [ false ] ] ];
		contextMenu: self argumentsListMenu.

]

{ #category : #accessing }
RBEnvironmentsPresenter >> initializeWidgets [
	scopeList := self newTreeTable.
]

{ #category : #actions }
RBEnvironmentsPresenter >> inspect: anItem [
	anItem inspect
]

{ #category : #accessing }
RBEnvironmentsPresenter >> orWithScope: anItem [ 
	self performOperation: #| with: anItem
]

{ #category : #accessing }
RBEnvironmentsPresenter >> performOperation: aSymbol with: anItem [
	| anEnvironment newScope |
	[ anEnvironment := self selectAnotherScope.
	newScope := anItem perform: aSymbol with: anEnvironment.
	(self confirm: 'Do you want store this result in the list?') 
		ifTrue: [
			RefactoringSettings addScope: newScope.
			self updateList.
		] ifFalse: [ newScope inspect ].
	] 
	on: Error 
	do: [ :e | self inform: 'Something was wrong']
]

{ #category : #actions }
RBEnvironmentsPresenter >> remove: anItem [
	RefactoringSettings removeScopeNamed: anItem label.
	self updateList 
]

{ #category : #actions }
RBEnvironmentsPresenter >> selectAnotherScope [ 
	| anotherScope list |
	list := RefactoringSettings scopeList.
	anotherScope := UIManager default
			chooseFrom: (list collect: #shortDescription)
			values: list
			title: 'Select another scope (RBEnvironment)'.
	^ anotherScope 
]

{ #category : #accessing }
RBEnvironmentsPresenter >> updateList [ 
	scopeList roots: RefactoringSettings scopeList
]
