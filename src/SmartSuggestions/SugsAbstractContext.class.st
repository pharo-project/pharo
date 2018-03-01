"
I'm the API used by smart suggestions with facilities to know the selected text, and having all the needed information for execute the commands.
"
Class {
	#name : #SugsAbstractContext,
	#superclass : #Object,
	#instVars : [
		'selectedNode',
		'model',
		'selectionPreviousHighligth'
	],
	#category : #'SmartSuggestions-Menu'
}

{ #category : #'instance creation' }
SugsAbstractContext class >> contextFor: anEditor [
	(anEditor respondsTo: #sugsContext)
		ifTrue: [ ^ anEditor sugsContext ].
	(anEditor model respondsTo: #interactionModel)
		ifTrue: [ 
			(anEditor model interactionModel respondsTo: #sugsContext)
				ifTrue: [ ^ anEditor model interactionModel sugsContext ] ].
	
	^ SugsNullContext new
]

{ #category : #build }
SugsAbstractContext class >> model: theModel [
	^ self new 
		model: theModel;
		yourself.
]

{ #category : #selection }
SugsAbstractContext >> bindedSelectedClass [
	^selectedNode binding value.
]

{ #category : #refactoring }
SugsAbstractContext >> browsedEnvironment [
	^RBBrowserEnvironment new
]

{ #category : #accessing }
SugsAbstractContext >> code [
	^self sourceTextArea text
]

{ #category : #refactoring }
SugsAbstractContext >> formatSourceCode [
	| formatted |

	formatted := selectedNode formattedCode.
	formatted = self code ifTrue: [ ^ self ].
	self sourceTextArea
		formatSourceCode;
		hasUnacceptedEdits: true
]

{ #category : #testing }
SugsAbstractContext >> hasClassUnImplementedAccessors [
	| hasGetter hasSetter name |

	name := self selectedVariableName asString copy.
	name at: 1 put: name first asLowercase .
	
	hasGetter := RBCondition canUnderstand: name asSymbol in: self selectedClass class.
	hasSetter := RBCondition canUnderstand: (name,':' ) asSymbol in: self selectedClass class.
	^ (hasGetter not | hasSetter not) check
]

{ #category : #testing }
SugsAbstractContext >> hasInstanceVariable [
	
	^(RBCondition hasInstanceVariable: self selectedClass) check.

]

{ #category : #testing }
SugsAbstractContext >> hasMoreSentences [
	^ selectedNode parent isNotNil and: [ selectedNode parent isMethod not ]
]

{ #category : #testing }
SugsAbstractContext >> hasOneStatement [
	^ (self selectedMessageName isNotNil or: [ selectedNode isLiteralNode ] ) or: [ selectedNode statements size = 1]
]

{ #category : #testing }
SugsAbstractContext >> hasParameters [
	
	^self selectedMessageName isNil or:[ self selectedMessageName numArgs > 0].

]

{ #category : #testing }
SugsAbstractContext >> hasSubclasses [
	
	^(RBCondition hasSubclasses: self selectedClass) check.

]

{ #category : #testing }
SugsAbstractContext >> hasUnImplementedAccessors [
	| hasGetter hasSetter |
	
	hasGetter := RBCondition canUnderstand: self selectedVariableName asSymbol in: self selectedClass.
	hasSetter := RBCondition canUnderstand: (self selectedVariableName,':' ) asSymbol in: self selectedClass.
	^ (hasGetter not | hasSetter not) check
]

{ #category : #testing }
SugsAbstractContext >> hasUnacceptedEdits [
	^self sourceTextArea hasUnacceptedEdits
]

{ #category : #testing }
SugsAbstractContext >> isMethodSelected [
	^selectedNode isMethod 
]

{ #category : #accessing }
SugsAbstractContext >> model [
	^ model
]

{ #category : #initialize }
SugsAbstractContext >> model: aModel [ 
	model := aModel
]

{ #category : #accessing }
SugsAbstractContext >> nodeStart [
	^selectedNode start.
]

{ #category : #accessing }
SugsAbstractContext >> nodeStop [
	^selectedNode stop.
]

{ #category : #selection }
SugsAbstractContext >> selectTheTextForTheNode [
	selectionPreviousHighligth := self selectedInterval .
	selectedNode ifNotNil: [ self sourceTextArea selectFrom: self nodeStart to: self nodeStop]
]

{ #category : #selection }
SugsAbstractContext >> selectedClass [
	self subclassResponsibility 
]

{ #category : #selection }
SugsAbstractContext >> selectedInterval [

	^ 1 to: 0
]

{ #category : #selection }
SugsAbstractContext >> selectedMessageName [
	^[selectedNode selector] on: MessageNotUnderstood do: [ nil ]
]

{ #category : #selection }
SugsAbstractContext >> selectedMethod [
	self subclassResponsibility 
]

{ #category : #initialize }
SugsAbstractContext >> selectedNode [
	^selectedNode 
]

{ #category : #initialize }
SugsAbstractContext >> selectedNode: theSelectedNode [
	selectedNode := theSelectedNode .
]

{ #category : #accessing }
SugsAbstractContext >> selectedNodeLabel [
	| label |
	selectedNode ifNil: [ ^ '' ].
	label := selectedNode sugsMenuLabel.
	^ label copyFrom: 1 to: (35 min: label size)
]

{ #category : #private }
SugsAbstractContext >> selectedStatements [
	selectedNode isMessage ifTrue: [ ^{selectedNode} ].
	^(selectedNode statements select: [ :stat | stat containedBy: selectionPreviousHighligth ]).
]

{ #category : #selection }
SugsAbstractContext >> selectedStatementsAsSequence [
	| statements temporaries  |
	
	temporaries :=  self selectedTemporaryVariables.
	statements:= self selectedStatements.
	^RBSequenceNode temporaries: temporaries statements: statements
	
]

{ #category : #private }
SugsAbstractContext >> selectedTemporaryVariables [
	selectedNode isSequence ifFalse: [ ^#() ].
	selectedNode temporaries ifEmpty: [^OrderedCollection new].
	^ ((selectedNode temporaries at: 1) containedBy: selectionPreviousHighligth )
		ifTrue: [ selectedNode temporaries ]
		ifFalse: [#()].
	
]

{ #category : #selection }
SugsAbstractContext >> selectedVariableName [
	^selectedNode name
]

{ #category : #accessing }
SugsAbstractContext >> sourceTextArea [
	self subclassResponsibility 
]

{ #category : #accessing }
SugsAbstractContext >> window [
	^ self sourceTextArea ownerThatIsA: SystemWindow
]
