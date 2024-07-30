Class {
	#name : 'StClassAndMethodsSelectionPresenter',
	#superclass : 'StRequestClassPresenter',
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

{ #category : 'accessing' }
StClassAndMethodsSelectionPresenter class >> defaultExtent [

	^ 500 @ 700
]

{ #category : 'initialization' }
StClassAndMethodsSelectionPresenter >> baseClassName: aString [
	"By default set a name close to the original one."

	textInput text: aString
]

{ #category : 'accessing' }
StClassAndMethodsSelectionPresenter >> basePackageName: aString [

 	packagePresenter selectItem: aString.
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
	"Answer the receiver's <SpTablePresenter> that holds the class methods"

	^ (methodsNotebookPresenter pageAt: 2) retrievePresenter
]

{ #category : 'layout' }
StClassAndMethodsSelectionPresenter >> defaultLayout [ 

	^ SpBoxLayout newTopToBottom 
		add: (SpGridLayout new 
			borderWidth: 5;
			beColumnNotHomogeneous;
			column: 2 expand: true;
			build: [ :builder | builder
				add: 'New class name: '; add: textInput; nextRow;
				add: 'Package'; add: packagePresenter; nextRow;
				add: 'Tag'; add: tagPresenter; nextRow ]) height: 100;
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
	self selectedClassMethods: OrderedCollection new
]

{ #category : 'initialization' }
StClassAndMethodsSelectionPresenter >> initializeClassNamePresenter [

	textInput := self newTextInput
		whenTextChangedDo: [ : text |
			(Smalltalk globals hasClassNamed: text asSymbol)
				ifTrue: [ self inform: 'Class ' , text asString , ' already exists' ]
				ifFalse: [  ] ];
		yourself
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
		whenSelectionChangedDo: [ : selection | self handleMethodChange: selection selectedItem compiledMethod ];
		contextMenu: [ (self rootCommandsGroup / 'StClassAndMethodsSelectionMenu') beRoot asMenuPresenter ];
		yourself
]

{ #category : 'initialization' }
StClassAndMethodsSelectionPresenter >> initializeCodePresenter [

	codePresenter := self newCode
		beWrapWord;
		yourself
]

{ #category : 'initialization' }
StClassAndMethodsSelectionPresenter >> initializeFocus [

	self focusOrder 
		add: textInput;
		add: packagePresenter;
		add: tagPresenter;
		add: codePresenter
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
			icon: (self iconNamed: #smallObjects) 
			provider: [ self initializeInstanceTablePresenter ])
				whenRetrievedDo: [ :presenter | self instanceMethods ifNotNil: [ : iMthds | presenter items: iMthds ] ]);
		addPage: ((SpNotebookPage 
			title: 'Class' 
			icon: (self iconNamed: #class) 
			provider: [ self initializeClassTablePresenter ])
				whenRetrievedDo: [ :presenter | self classMethods ifNotNil: [ : iMthds | presenter items: iMthds ] ]);
		yourself
]

{ #category : 'initialization' }
StClassAndMethodsSelectionPresenter >> initializePresenters [

	super initializePresenters.
	self
		initializeNotebookPresenter;
		initializeCodePresenter.
	methodsNotebookPresenter update.
	self
		acceptLabel: 'Create';
		validateAnswer: self validateAnswerBlock;
		initializeFocus
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

{ #category : 'accessing' }
StClassAndMethodsSelectionPresenter >> methodsNotebookPresenter [

	^ methodsNotebookPresenter
]

{ #category : 'accessing' }
StClassAndMethodsSelectionPresenter >> methodsNotebookPresenter: anObject [

	methodsNotebookPresenter := anObject
]

{ #category : 'api - selection' }
StClassAndMethodsSelectionPresenter >> selectAll [
	"Select all instance and class methods in the receiver"

	self selectedInstanceMethods: self instanceMethods.
	self selectedClassMethods: self classMethods.

]

{ #category : 'accessing' }
StClassAndMethodsSelectionPresenter >> selectedClassMethods [

	^ selectedClassMethods
		ifNil: [ selectedClassMethods := OrderedCollection new ]
]

{ #category : 'accessing' }
StClassAndMethodsSelectionPresenter >> selectedClassMethods: anObject [

	selectedClassMethods := anObject
]

{ #category : 'accessing' }
StClassAndMethodsSelectionPresenter >> selectedInstanceMethods [

	^ selectedInstanceMethods
		ifNil: [ selectedInstanceMethods := OrderedCollection new ]
]

{ #category : 'accessing' }
StClassAndMethodsSelectionPresenter >> selectedInstanceMethods: anObject [

	selectedInstanceMethods := anObject
]

{ #category : 'initialization' }
StClassAndMethodsSelectionPresenter >> setTitleTo: aSpWindowPresenter [

	aSpWindowPresenter title: 'Copy class ' , driver className
]

{ #category : 'initialization' }
StClassAndMethodsSelectionPresenter >> setWindowIconTo: aSpWindowPresenter [

	aSpWindowPresenter windowIcon: (self iconNamed: #smallCopy)
]

{ #category : 'initialization' }
StClassAndMethodsSelectionPresenter >> withInstanceMethods: instMethods withClassMethods: clsMethods [

	self instanceMethods: instMethods.
	self classMethods: clsMethods
]
