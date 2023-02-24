Class {
	#name : #RBEnvironmentsPresenter,
	#superclass : #SpPresenter,
	#instVars : [
		'scopeList'
	],
	#category : #'SystemCommands-RefactoringSupport-Scope'
}

{ #category : #layout }
RBEnvironmentsPresenter class >> defaultLayout [
	^ SpBoxLayout newTopToBottom
		add:
			(SpBoxLayout newLeftToRight
				add: #scopeList;
				yourself);
		yourself
]

{ #category : #specs }
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
	self new open
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
						name: 'NOT';
						action: [ self notWithScope: scopeList selectedItem ] ];
				addItem: [ :anItem |
					anItem
						name: 'Inspect';
						visibleIf: [ (scopeList selectedItem isCompositeEnvironment not
							and: [ scopeList selectedItem label ~= 'current image' ])];
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

{ #category : #initialization }
RBEnvironmentsPresenter >> initializePresenter [

	scopeList addColumn: (SpStringTableColumn title: 'Environment' evaluated: #shortDescription);
		roots: RefactoringSettings scopeList;
		children: [ :aClass | RefactoringSettings scopeList
			select: [ :cls | aClass isCompositeEnvironment ifTrue:
				[ aClass environment == cls or: [ aClass otherEnvironment == cls ] ]
			ifFalse: [ false ] ] ];
		contextMenu: self argumentsListMenu
]

{ #category : #initialization }
RBEnvironmentsPresenter >> initializeWidgets [
	scopeList := self newTreeTable
]

{ #category : #actions }
RBEnvironmentsPresenter >> inspect: anItem [

	(anItem inspectIn: self) open title: anItem shortDescription
]

{ #category : #actions }
RBEnvironmentsPresenter >> notWithScope: anItem [

	[ | newScope |
	newScope := anItem not.
	(self confirm: 'Do you want store this result in the list?')
		ifTrue: [
			RefactoringSettings addScope: newScope.
			self updateList.
		] ifFalse: [ newScope inspect ] ]
	on: Error
	do: [ :e | self inform: 'Something was wrong']
]

{ #category : #actions }
RBEnvironmentsPresenter >> orWithScope: anItem [
	self performOperation: #| with: anItem
]

{ #category : #actions }
RBEnvironmentsPresenter >> performOperation: aSymbol with: anItem [
	| anEnvironment newScope |
	[ anEnvironment := self selectAnotherScopeWhitout: anItem.
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
	RefactoringSettings removeScopeNamed: anItem description.
	self updateList
]

{ #category : #actions }
RBEnvironmentsPresenter >> selectAnotherScopeWhitout: anItem [
	| anotherScope list |
	list := RefactoringSettings scopeList copyWithout: anItem.
	anotherScope := UIManager default
			chooseFrom: (list collect: #shortDescription)
			values: list
			title: 'Select another scope (RBEnvironment)'.
	^ anotherScope
]

{ #category : #actions }
RBEnvironmentsPresenter >> updateList [
	scopeList roots: RefactoringSettings scopeList
]
