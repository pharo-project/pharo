"
I am an UI used to remove method arguments and optionally rename methods.

###How to run

```
SycMethodArgumentsEditorPresenter
		openOn:
			(RBMethodName
				selector: (RBExtractMethodRefactoring >> #validateRenameOf:to:) selector
				arguments: ((RBExtractMethodRefactoring >> #validateRenameOf:to:) ast arguments collect: #name))
```				

###My jobs are:
	- display the arguments list of your method and let you select the one you want to eliminate 
	- allow you to change the method's selector after removing the argument
	- display a preview of your future method name or '(invalid)' when the name is incorrect  (with a LabelPresenter previewResult an instance variable)
	
###Public API and Key Messages

See class side 'example' script to know how to use me.
"
Class {
	#name : #SycMethodArgumentsRemoverPresenter,
	#superclass : #SpPresenter,
	#instVars : [
		'selectorInput',
		'argumentsList',
		'previewResult',
		'methodName',
		'args'
	],
	#category : #'SystemCommands-RefactoringSupport-View'
}

{ #category : #layout }
SycMethodArgumentsRemoverPresenter class >> defaultLayout [

	<spec>
	^ SpBoxLayout newTopToBottom
		  add: (SpBoxLayout newTopToBottom
				   add: 'Argument to be removed';
				   add: #argumentsList;
				   yourself);
		  add: 'Modified selector';
		  add: #selectorInput;
		  add: 'Preview';
		  add: #previewResult;
		  yourself
]

{ #category : #examples }
SycMethodArgumentsRemoverPresenter class >> example [
	<script>
	self
		openOn:
			(RBMethodName
				selector: (RBExtractMethodRefactoring >> #validateRenameOf:to:) selector
				arguments: ((RBExtractMethodRefactoring >> #validateRenameOf:to:) ast arguments collect: [:each | each name]))
]

{ #category : #'instance creation' }
SycMethodArgumentsRemoverPresenter class >> openOn: aMethod [
	"I take a RBMethodName as parameter and open the refactoring UI in a modal to remove arguments and optionally rename it."
	|temp|
	temp := self on: aMethod.
	^ temp openBlockedDialog
]

{ #category : #initialization }
SycMethodArgumentsRemoverPresenter >> connectPresenters [

	selectorInput whenTextChangedDo: [ :text | self updateLabel ].
	argumentsList whenSelectionChangedDo: [ :selection |
		selection selectedItem ifNotNil: [
			selectorInput text:
				(self defaultSelectorAfterRemoving: selection selectedItem name) ] ].
	argumentsList displayColor: [ :arg |
		arg = argumentsList selection selectedItem
			ifTrue: [ Color lightGray ]
			ifFalse: [ self theme textColor ] ].
	argumentsList items:
		(methodName arguments collect: [ :arg | RBArgumentName name: arg ]).
	argumentsList items ifNotEmpty: [ argumentsList selectIndex: 1 ]
]

{ #category : #action }
SycMethodArgumentsRemoverPresenter >> defaultSelectorAfterRemoving: argumentName [

	| argIndex keywords |
	argIndex := methodName arguments indexOf: argumentName.
	keywords := methodName selector keywords asOrderedCollection.
	keywords size = 1 ifTrue: [
		^ (keywords first copyWithout: $:) asSymbol ].
	keywords removeAt: argIndex.
	^ (keywords joinUsing: '') asSymbol
]

{ #category : #accessing }
SycMethodArgumentsRemoverPresenter >> getParametersOrder [

	^ argumentsList items
		  reject: [ :arg | arg = argumentsList selection selectedItem ]
		  thenCollect: [ :arg | arg newName ]
]

{ #category : #initialization }
SycMethodArgumentsRemoverPresenter >> initializeDialogWindow: aModalPresenter [

	aModalPresenter
		addButton: 'Accept' do: [ :presenter | self renameMethodAndClose: presenter ];
		addButton: 'Cancel' do: [ :presenter | presenter beCancel; close ];
		initialExtent: 300 @ 300
]

{ #category : #initialization }
SycMethodArgumentsRemoverPresenter >> initializePresenters [

	selectorInput := self newTextInput.
	argumentsList := self newList.
	previewResult := self newLabel.

	selectorInput autoAccept: true.
	selectorInput text: methodName selector.
	previewResult label: methodName methodName.

	self setFocus
]

{ #category : #action }
SycMethodArgumentsRemoverPresenter >> renameMethodAndClose: presenter [
	^ previewResult label = '(invalid)'
		ifTrue: [ self inform: 'Invalid method name' ]
		ifFalse: [
			methodName
				arguments: self getParametersOrder;
				selector: selectorInput text.
			presenter
				beOk;
				close ]
]

{ #category : #action }
SycMethodArgumentsRemoverPresenter >> setFocus [

	self focusOrder
		add: argumentsList;
		add: selectorInput
]

{ #category : #'accessing - model' }
SycMethodArgumentsRemoverPresenter >> setModelBeforeInitialization: aRBMethodName [

	methodName := aRBMethodName.
	args := methodName arguments copy
]

{ #category : #action }
SycMethodArgumentsRemoverPresenter >> updateLabel [

	"Update the new method name to display to the user when the user change its name or order of the arguments."

	previewResult label: (RBMethodName
			 selector: (selectorInput text ifNil: [ '' ])
			 arguments: (argumentsList items
					  reject: [ :arg | argumentsList selection selectedItem = arg ]
					  thenCollect: #newName)) methodName
]
