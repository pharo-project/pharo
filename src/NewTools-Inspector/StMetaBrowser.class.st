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
		lineNumbers: true;
		"contextMenu: [ self menuActions asMenuPresenter ];
		contextKeyBindings: self menuActions asKMCategory;"
		beForMethod: self selectedMethod;
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
StMetaBrowser >> selectedMethod [

	^ methods selection selectedItem
]

{ #category : #accessing }
StMetaBrowser >> selectedObject [

	^ self selectedMethod
]

{ #category : #'private updating' }
StMetaBrowser >> updateMethods: classTreeSelection [
	| class |

	class := classTreeSelection selectedItem.
	class
		ifNil: [ methods items: SortedCollection new ]
		ifNotNil: [ | sortedMethods |
			sortedMethods := SortedCollection
				sortBlock: [ :a :b | a selector <= b selector ].
			sortedMethods addAll: class methods.
			methods items: sortedMethods ].
	methods unselectAll
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
