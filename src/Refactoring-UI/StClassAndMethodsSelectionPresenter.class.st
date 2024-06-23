Class {
	#name : 'StClassAndMethodsSelectionPresenter',
	#superclass : 'StMethodsSelectionPresenter',
	#instVars : [
		'selectedInstanceMethods',
		'selectedClassMethods',
		'instanceMethods',
		'classMethods',
		'methodsNotebookPresenter',
		'codePresenter'
	],
	#category : 'Refactoring-UI-UI',
	#package : 'Refactoring-UI',
	#tag : 'UI'
}

{ #category : 'accessing' }
StClassAndMethodsSelectionPresenter class >> buildCommandsGroupWith: presenterInstance forRoot: rootCommandGroup [

	rootCommandGroup
		register: (self buildStMethodsSelectionContextualGroupWith: presenterInstance)
]

{ #category : 'accessing' }
StClassAndMethodsSelectionPresenter class >> buildStMethodsSelectionContextualGroupWith: presenterInstance [

	^ (CmCommandGroup named: 'StClassAndMethodsSelectionMenu') asSpecGroup
		register: ((CmCommandGroup named: 'StClassAndMethodsSelectionOperationsMenu') asSpecGroup
			register: ((StClassAndMethodsSelectAllCommand
				forSpecWithIconNamed: #checkedBox
				shortcutKey: 'A' asShortcut) 
					context: presenterInstance);			
			register: ((StClassAndMethodsSelectNoneCommand 
				forSpecWithIconNamed: #checkboxUnselected
				shortcutKey: 'N' asShortcut)
					context: presenterInstance);
			beDisplayedAsGroup;
			yourself);
		yourself
]

{ #category : 'examples' }
StClassAndMethodsSelectionPresenter class >> example [

	(self new 
		label: 'Example' 
		withInstanceMethods: Object methods 
		withClassMethods: {}) open

]

{ #category : 'examples' }
StClassAndMethodsSelectionPresenter class >> exampleAllInstanceMethodsSelected [

	(self new 
		label: 'Example' 
		withInstanceMethods: Object methods 
		withClassMethods: Object class methods) open
]

{ #category : 'accessing' }
StClassAndMethodsSelectionPresenter >> classMethods [

	^ classMethods
]

{ #category : 'accessing' }
StClassAndMethodsSelectionPresenter >> classMethods: anObject [

	classMethods := anObject
]

{ #category : 'private' }
StClassAndMethodsSelectionPresenter >> classMethodsTable [

	^ (methodsNotebookPresenter pageAt: 2) retrievePresenter
]

{ #category : 'layout' }
StClassAndMethodsSelectionPresenter >> defaultLayout [ 

	^ SpBoxLayout newTopToBottom 
		add: (SpPanedLayout newLeftToRight 
			add: methodsNotebookPresenter;
			add: codePresenter;
			yourself);
		yourself
]

{ #category : 'event handling' }
StClassAndMethodsSelectionPresenter >> handleMethodChange: aCompiledMethod [ 

	codePresenter 
		beForMethod: aCompiledMethod;
		text: aCompiledMethod sourceCode
	
]

{ #category : 'initialization' }
StClassAndMethodsSelectionPresenter >> initialize [

	super initialize.
	self selectedInstanceMethods: OrderedCollection new.
	self selectedClassMethods: OrderedCollection new.
]

{ #category : 'initialization' }
StClassAndMethodsSelectionPresenter >> initializeClassTablePresenter [

	^ self newTable	
		beMultipleSelection;
		addColumn: ((SpCheckBoxTableColumn title: '' evaluated: [ : im | self selectedClassMethods includes: im ])
			onActivation: [ : cv | ];
			onDeactivation: [ : cv |  ];
			beNotExpandable;
			width: 20;
			yourself);
		addColumn: ((SpStringTableColumn title: 'Name' evaluated: [ : im | im selector asString ])
			width: 180;
			yourself);
		sortingBlock: [ :a :b | a selector < b selector ];
		whenSelectionChangedDo: [ : selection | self handleMethodChange: selection selectedItem ];
		contextMenu: [ (self rootCommandsGroup / 'StClassAndMethodsSelectionMenu') beRoot asMenuPresenter ];	
		"searchMatching: [ :selFormula :textToSearch | selFormula selector beginsWith: textToSearch ];"
		yourself
]

{ #category : 'initialization' }
StClassAndMethodsSelectionPresenter >> initializeCodePresenter [

	codePresenter := self newCode
		beWrapWord;
		yourself
]

{ #category : 'initialization' }
StClassAndMethodsSelectionPresenter >> initializeInstanceTablePresenter [

	^ self newTable	
		beMultipleSelection;
		addColumn: ((SpCheckBoxTableColumn title: '' evaluated: [ : im | self selectedInstanceMethods includes: im ])
			onActivation: [ : cv | ];
			onDeactivation: [ : cv |  ];
			beNotExpandable;
			width: 20;
			yourself);
		addColumn: ((SpStringTableColumn title: 'Name' evaluated: [ : im | im selector asString ])
			width: 180;
			yourself);
		sortingBlock: [ :a :b | a selector < b selector ];
		whenSelectionChangedDo: [ : selection | self handleMethodChange: selection selectedItem compiledMethod ];
		contextMenu: [ (self rootCommandsGroup / 'StClassAndMethodsSelectionMenu') beRoot asMenuPresenter ];			
		"searchMatching: [ :selFormula :textToSearch | selFormula selector beginsWith: textToSearch ];"
		yourself.
]

{ #category : 'initialization' }
StClassAndMethodsSelectionPresenter >> initializeNotebookPresenter [

	methodsNotebookPresenter := self newNotebook
		addPage: ((SpNotebookPage 
			title: 'Instance' 
			icon: (self iconNamed: #smallPaint) 
			provider: [ self initializeInstanceTablePresenter ])
				whenRetrievedDo: [ :presenter | self instanceMethods ifNotNil: [ : iMthds | presenter items: iMthds ] ]);
		addPage: ((SpNotebookPage 
			title: 'Class' 
			icon: (self iconNamed: #smallPaint) 
			provider: [ self initializeClassTablePresenter ])
				whenRetrievedDo: [ :presenter | self classMethods ifNotNil: [ : iMthds | presenter items: iMthds ] ]);
		yourself
]

{ #category : 'initialization' }
StClassAndMethodsSelectionPresenter >> initializePresenters [

	self 
		initializeNotebookPresenter;
		initializeCodePresenter
]

{ #category : 'accessing' }
StClassAndMethodsSelectionPresenter >> instanceMethods [

	^ instanceMethods
]

{ #category : 'accessing' }
StClassAndMethodsSelectionPresenter >> instanceMethods: anObject [

	instanceMethods := anObject
]

{ #category : 'private' }
StClassAndMethodsSelectionPresenter >> instanceMethodsTable [
	"Answer the receiver's <SpTablePresenter> that holds the instance methods"

	^ (methodsNotebookPresenter pageAt: 1) retrievePresenter
]

{ #category : 'testing' }
StClassAndMethodsSelectionPresenter >> isInstanceMethodsSelected [
	"Answer <true> if the receiver's tool is displaying the instance methods page"
	
	^ methodsNotebookPresenter selectedPage title = 'Instance'
]

{ #category : 'initialization' }
StClassAndMethodsSelectionPresenter >> label: aString withInstanceMethods: instMethods withClassMethods: clsMethods [

	label label: aString.

	self instanceMethods: instMethods.
	self classMethods: clsMethods.

	self selectedInstanceMethods: instMethods.
	self selectedClassMethods: clsMethods.

	methodsNotebookPresenter update.
]

{ #category : 'accessing' }
StClassAndMethodsSelectionPresenter >> methodsNotebookPresenter [

	^ methodsNotebookPresenter
]

{ #category : 'accessing' }
StClassAndMethodsSelectionPresenter >> methodsNotebookPresenter: anObject [

	methodsNotebookPresenter := anObject
]

{ #category : 'accessing' }
StClassAndMethodsSelectionPresenter >> selectedClassMethods [

	^ selectedClassMethods
]

{ #category : 'accessing' }
StClassAndMethodsSelectionPresenter >> selectedClassMethods: anObject [

	selectedClassMethods := anObject
]

{ #category : 'accessing' }
StClassAndMethodsSelectionPresenter >> selectedInstanceMethods [

	^ selectedInstanceMethods
]

{ #category : 'accessing' }
StClassAndMethodsSelectionPresenter >> selectedInstanceMethods: anObject [

	selectedInstanceMethods := anObject
]
