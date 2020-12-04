Class {
	#name : #StMetaBrowser,
	#superclass : #StInspection,
	#instVars : [
		'classes',
		'methods',
		'source',
		'methodFilter'
	],
	#category : #'NewTools-Inspector-View'
}

{ #category : #commands }
StMetaBrowser class >> buildCommandsGroupWith: presenter forRoot: aCmCommandsGroup [

	aCmCommandsGroup
		beDisplayedAsGroup;
		register: (StInspectorInspectSelectedCommand forSpecContext: presenter);
		register: (StInspectorBrowseSelectedCommand forSpecContext: presenter)
]

{ #category : #specs }
StMetaBrowser class >> defaultSpec [
	
	^ SpPanedLayout newVertical
		add: (SpBoxLayout newHorizontal
			spacing: 2;
			add: #classes;
			add: (SpBoxLayout newVertical
				spacing: 2;
				add: #methods;
				addLast: #methodFilter expand: false fill: false padding: 0;
				yourself);
			yourself);
		add: #source;
		yourself
]

{ #category : #'private actions' }
StMetaBrowser >> compile: aString [
	| compiledSelector cursorPosition |

	cursorPosition := source cursorPositionIndex.

	compiledSelector := self selectedMethod methodClass
		compile: aString
		classified: 	self selectedMethod protocol.
	
	self updateMethods: self selectedClass.
	methods items 
		detect: [ :each | each selector = compiledSelector ]
		ifFound: [ :aMethod | 
			methods selectItem: aMethod.
			source cursorPositionIndex: cursorPosition ]
]

{ #category : #initialization }
StMetaBrowser >> initializePresenters [
	| classHierarchy |
	
	classHierarchy := self model class withAllSuperclasses reversed.

	classes := self newTreeTable
		addColumn: (SpStringTableColumn title: 'Class hierarchy' evaluated: #name);
		roots: { classHierarchy first };
		children: [ :aClass | classHierarchy select: [ :subclass | subclass superclass == aClass ] ];
		yourself.
	methods := self newList
		display: [ :each | each selector ];	
		contextMenu: [ self rootCommandsGroup asMenuPresenter ];
		yourself.
	methodFilter := self newTextInput.

	source := self newCode
		lineNumbers: StPharoSettings codeShowLineNumbers;
		beForMethod: self selectedMethod;
		whenSubmitDo: [ :aString | self compile: aString ];
		whenResetDo: [ self updateSourceWith: self selectedMethod  ];
		yourself.
		
	classes transmitTo: methods transform: [ :aClass | self methodsOf: aClass ].
	methods transmitDo: [ :aMethod | self updateSourceWith: aMethod ].

	classes selectPath: (Array new: classHierarchy size withAll: 1)
]

{ #category : #private }
StMetaBrowser >> methodsOf: aClass [

	aClass ifNil: [ ^ #() ].
	^ aClass methods sorted: #selector ascending

]

{ #category : #accessing }
StMetaBrowser >> selectedClass [

	^ classes selection selectedItem
]

{ #category : #accessing }
StMetaBrowser >> selectedMethod [

	^ methods selection selectedItem
]

{ #category : #accessing }
StMetaBrowser >> selectedObject [

	^ self selectedMethod
]

{ #category : #'private updating' }
StMetaBrowser >> updateMethods: aClass [
	
	methods items: (aClass
		ifNotNil: [ aClass methods sorted: #selector ascending ]
		ifNil: [ #() ])
	
]

{ #category : #'private updating' }
StMetaBrowser >> updateSourceWith: method [

	source userActionDuring: [
		method
			ifNotNil: [ 
				source text: method sourceCode; beForMethod: method  ]
			ifNil: [ 
				source text: ''; clearInteractionModel ] ]
]
