"
I am a base class for method edito tools.

I implement correctly the styling of typed source code. 
Also I add multiple text editing tools to the status bar: 
- wrap mode switch 
- line number=s switch
- format as you read 
- method tags and package editor.

In addition to the superclass abstract methods subclasses should implement following methods: 

- methodClass 
Subclasses should decide what class will accept editing method

- modifiesExtension 
Subclasses should detect that editing method is going to be extension.

Internal Representation and Key Implementation Points.

    Instance Variables
	extendingPackage:		<Package>
	methodProtocol:		<Symbol> represents the name of the method protocol
"
Class {
	#name : 'ClyMethodEditorToolMorph',
	#superclass : 'ClyTextEditorToolMorph',
	#instVars : [
		'ast',
		'methodProtocol',
		'extendingPackage',
		'targetClasses'
	],
	#category : 'Calypso-SystemTools-Core-Editors-Methods',
	#package : 'Calypso-SystemTools-Core',
	#tag : 'Editors-Methods'
}

{ #category : 'testing' }
ClyMethodEditorToolMorph class >> isAbstract [
	^self = ClyMethodEditorToolMorph
]

{ #category : 'accessing' }
ClyMethodEditorToolMorph class >> tabOrder [
	^20
]

{ #category : 'testing' }
ClyMethodEditorToolMorph >> allowsDifferentActivationPriority [
	^false
]

{ #category : 'testing' }
ClyMethodEditorToolMorph >> belongsToRemovedBrowserContext [
	super belongsToRemovedBrowserContext ifTrue: [ ^true ].

	^self methodClass isObsolete
]

{ #category : 'building' }
ClyMethodEditorToolMorph >> buildTextMorph [
	super buildTextMorph.
	ast := self initializeAST.
	self formatTextIfNeeded
]

{ #category : 'operations' }
ClyMethodEditorToolMorph >> chooseClassForNewMethodIfNone: aBlock [

	targetClasses size = 1 ifTrue: [ ^ targetClasses first].

	^[self browser searchDialog
			requestSingleObject: 'Where to install new method?'
			from: (ClyAllClassesQuery as: ClyMethodVisibilityProviderAnnotation defaultHierarchy asQueryResult)
			inScope: (ClyClassScope ofAll: targetClasses)]
		on: CmdCommandAborted, CmCommandAborted do: [:err | aBlock value ]
]

{ #category : 'events handling' }
ClyMethodEditorToolMorph >> currentEditedAST [

	^ self methodClass compiler
		  source: self pendingText asString;
		  parse
]

{ #category : 'operations' }
ClyMethodEditorToolMorph >> editProtocolOf: aMethod [

	self applyChangesBy: [ methodProtocol ifNotNil: [ :protocolName | aMethod protocol: protocolName ] ]
]

{ #category : 'accessing' }
ClyMethodEditorToolMorph >> editingClass [
	^self methodClass
]

{ #category : 'accessing' }
ClyMethodEditorToolMorph >> extendingPackage [
	^ extendingPackage
]

{ #category : 'accessing' }
ClyMethodEditorToolMorph >> extendingPackage: aPackage [

	extendingPackage := aPackage.
	methodProtocol := nil
]

{ #category : 'building' }
ClyMethodEditorToolMorph >> fillStatusBar [
	| position |
	super fillStatusBar.

	position := statusBar addContextItem: (ClyCursorPositionLabelMorph of: textMorph).
	position comeToFront.
	statusBar addCommandItem: (ClyTextWrapModeSwitchMorph of: textMorph).
	statusBar addCommandItem: (ClyTextLineNumbersSwitchMorph of: textMorph).
	statusBar addCommandItem: (ClyFormatAsReadSwitchMorph of: textMorph)
]

{ #category : 'operations' }
ClyMethodEditorToolMorph >> formatSourceCode [
	textMorph formatSourceCode
]

{ #category : 'building' }
ClyMethodEditorToolMorph >> formatTextIfNeeded [
	ASTProgramNode formatterClass formatAsYouReadPolicy ifFalse: [ ^self].

	textMorph formatSourceCode.
	textModel clearUserEdits
]

{ #category : 'initialization' }
ClyMethodEditorToolMorph >> initializeAST [
	"subclasses might get it from the method"
	^ self currentEditedAST
]

{ #category : 'testing' }
ClyMethodEditorToolMorph >> isCommandAvailable: aCommand [

	^ aCommand canBeExecutedInMethodEditor: self
]

{ #category : 'rubric interaction model' }
ClyMethodEditorToolMorph >> isScripting [
	^false
]

{ #category : 'accessing' }
ClyMethodEditorToolMorph >> methodClass [
	self subclassResponsibility
]

{ #category : 'accessing' }
ClyMethodEditorToolMorph >> methodProtocol [

	^ methodProtocol
]

{ #category : 'accessing' }
ClyMethodEditorToolMorph >> methodProtocol: protocolName [

	methodProtocol := protocolName.
	extendingPackage := nil
]

{ #category : 'testing' }
ClyMethodEditorToolMorph >> modifiesExtension [
	"it should answer if tool is going to modify extension state of method.
	For example tool ca move method to another package
	ot it can move extension method back to class package"
	self subclassResponsibility
]

{ #category : 'operations' }
ClyMethodEditorToolMorph >> packageEditingMethod: aMethod [

	self applyChangesBy: [
		(extendingPackage isNotNil and: [aMethod package ~~ extendingPackage]) ifTrue: [
			^(SycMoveMethodsToPackageCommand for: {aMethod} to: extendingPackage)
				execute].

		(extendingPackage isNil and: [ aMethod isExtension ]) ifTrue: [
			(SycMoveMethodsToTheDefiningClassCommand for: {aMethod})
				execute]
	]
]

{ #category : 'operations' }
ClyMethodEditorToolMorph >> protocolAndPackageEditingMethod: aMethod [
	self editProtocolOf: aMethod.
	"Classification should be performed before package movement
	because extending package is still based on protocol begining with star.
	So it should override manual classification"
	self packageEditingMethod: aMethod
]

{ #category : 'accessing' }
ClyMethodEditorToolMorph >> protocolAndPackageEditor [
	^(statusBar findDeeplyA: ClyProtocolEditorMorph) ifNil: [ CmdCommandAborted signal ]
]

{ #category : 'rubric interaction model' }
ClyMethodEditorToolMorph >> selectedClassOrMetaClass [
	^self methodClass
]

{ #category : 'initialization' }
ClyMethodEditorToolMorph >> setUpParametersFromModel [
	super setUpParametersFromModel.

	self setUpTargetClasses
]

{ #category : 'initialization' }
ClyMethodEditorToolMorph >> setUpTargetClasses [
	self subclassResponsibility
]

{ #category : 'events handling' }
ClyMethodEditorToolMorph >> textChanged: aTextChanged [

	super textChanged: aTextChanged.
	textMorph segments copy do: #delete.
	IconStyler new
	   stylerClasses: {ErrorNodeStyler . SemanticMessageIconStyler . SemanticWarningIconStyler };
		styleText: textModel withAst: self currentEditedAST.
	^ ast
]
