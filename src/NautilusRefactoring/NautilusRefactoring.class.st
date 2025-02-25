"
NautilusRefactoring is a facade for refactorings:
- class
- method
- inst var
- class var
- source code
"
Class {
	#name : 'NautilusRefactoring',
	#superclass : 'Object',
	#instVars : [
		'model'
	],
	#category : 'NautilusRefactoring-Base',
	#package : 'NautilusRefactoring',
	#tag : 'Base'
}

{ #category : 'instance creation' }
NautilusRefactoring class >> model: model [

	^ self new
		model: model;
		yourself
]

{ #category : 'display' }
NautilusRefactoring >> chooseFrom: anArray title: aString lines: aCollection [
	anArray isEmpty
		ifTrue: [ ^ nil ].
	anArray size = 1
		ifTrue: [ ^ anArray first ].
	^ self morphicUIManager chooseOrRequestFrom: anArray lines: aCollection title: aString
]

{ #category : 'display' }
NautilusRefactoring >> handleMethodNameRequest: aMethodName [
	|dialog|
	dialog := StMethodNameEditorPresenter
		openOn: aMethodName
		canRenameArgs: false
		canRemoveArgs: false
		canAddArgs: false.
	dialog cancelled ifTrue: [  CmdCommandAborted signal ].
	^ aMethodName
]

{ #category : 'display' }
NautilusRefactoring >> handleWarning: anException [
	self inform: anException messageText.
	anException resume
]

{ #category : 'accessing' }
NautilusRefactoring >> model [
	^ model
]

{ #category : 'accessing' }
NautilusRefactoring >> model: aNautilusUI [
	model := aNautilusUI
]

{ #category : 'display' }
NautilusRefactoring >> morphicUIManager [

	^ MorphicUIManager new
]

{ #category : 'option' }
NautilusRefactoring >> refactoringOptions: aRefactoring [
	aRefactoring
		setOption: #implementorToInline
		toUse: [ :ref :imps | self requestImplementorToInline: imps ];
		setOption: #methodName
		toUse: [ :name | self requestMethodNameFor: name ];
		setOption: #selfArgumentName
		toUse: [ :ref | self requestSelfArgumentName ];
		setOption: #selectVariableToMoveTo
		toUse: [ :ref :class :selector | self selectVariableToMoveMethodTo: selector class: class ];
		setOption: #variableTypes
		toUse: [ :ref :types :selected | self selectVariableTypesFrom: types selected: selected for: ref ];
		setOption: #extractAssignment
		toUse: [ :ref :string | self shouldExtractAssignmentTo: string ];
		setOption: #shouldNotCreateExtraBindings
		toUse: [ :ref :string | self shouldNotCreateExtraBindings: string ];
		setOption: #alreadyDefined
		toUse: [ :ref :class :selector | self shouldOverride: selector in: class ];
		setOption: #searchInWholeHierarchy
		toUse: [ :ref :class | self shouldSearchInHierarchy: class ]
]

{ #category : 'display' }
NautilusRefactoring >> request: aString [
	^ self request: aString initialAnswer: ''
]

{ #category : 'display' }
NautilusRefactoring >> request: requestString initialAnswer: initialAnswerString [

	| answer |
	answer := self morphicUIManager
		          request: requestString
		          initialAnswer: initialAnswerString.
	answer isEmptyOrNil ifTrue: [ RefactoringAborted signal ].
	^ answer
]

{ #category : 'option' }
NautilusRefactoring >> requestImplementorToInline: imps [
	^ self chooseFrom: imps title: 'Choose the class that should provide the implementation.' lines:#()
]

{ #category : 'option' }
NautilusRefactoring >> requestMethodNameFor: aMethodName [
	^ self handleMethodNameRequest: aMethodName
]

{ #category : 'option' }
NautilusRefactoring >> requestSelfArgumentName [
	^ self request: 'Enter name for argument to refer to "self" in extracted method'
]

{ #category : 'option' }
NautilusRefactoring >> selectVariableToMoveMethodTo: aSelector class: aClass [

	| parseTree nameList |

	parseTree := aClass parseTreeForSelector: aSelector.
	parseTree
		ifNil: [ parseTree := OCMethodNode selector: #value body: ( OCSequenceNode statements: #() ) ].
	nameList := OrderedCollection new.
	nameList
		addAll: parseTree argumentNames sorted;
		addAll: aClass allInstanceVariableNames sorted.
	^ self
		chooseFrom: nameList
		title: 'Select variable to move method into:'
		lines: ( Array with: parseTree argumentNames size )
]

{ #category : 'option' }
NautilusRefactoring >> selectVariableTypesFrom: aCollectionOfTypes selected: aSelectedCollection for: aRefactoring [
	| stream result |
	stream := WriteStream on: String new.
	aCollectionOfTypes do: [ :each | stream nextPutAll: each name ] separatedBy: [ stream cr ].
	result := Smalltalk ui theme
		textEditorIn: self model window
		text: 'Select classes to move to: (one class per line)'
		title: 'Select classes to move to:'
		entryText: stream contents
		entryHeight: 300.
	result ifNil: [ ^ nil ].
	^ (result asString findTokens: String crlf)
		collect: [ :each | aRefactoring model classFor: (Smalltalk classNamed: each trimBoth) ]
]

{ #category : 'option' }
NautilusRefactoring >> shouldExtractAssignmentTo: aString [
	^ self confirm: ('Do you want to extract the assignment of <1s> at the end of selection?' expandMacrosWith: aString)
]

{ #category : 'option' }
NautilusRefactoring >> shouldNotCreateExtraBindings: aString [

	^ (self confirm:
		   ('Do you want to extract the expression ''<1s>'' into a variable in the current method?'
			    expandMacrosWith: aString)) not
]

{ #category : 'option' }
NautilusRefactoring >> shouldOverride: aSelector in: aClass [
	^ self confirm: ('<1s> is already defined in the <2p> hierarchy. Extracting it to an existing selector may change behavior. Do you wish to use <1s> anyway?' expandMacrosWith: aSelector with: aClass)
]

{ #category : 'option' }
NautilusRefactoring >> shouldSearchInHierarchy: class [

	^ class subclasses
		  ifNotEmpty: [
			  self morphicUIManager
				  confirm:
				  'Do you want to search occurrences in the whole hierarchy?'
				  label: 'Warning' ]
		  ifEmpty: [ false ]
]
