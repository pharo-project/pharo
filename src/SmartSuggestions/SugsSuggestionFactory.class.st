"
I'm the guy who knows how to create the suggestions. 
I also categorize them.
"
Class {
	#name : #SugsSuggestionFactory,
	#superclass : #Model,
	#classInstVars : [
		'collectorForInstancesVariable',
		'collectorForTemporaryVariable',
		'collectorForUndeclaredVariable',
		'collectorForSourceCode',
		'collectorForMethod',
		'collectorForMessage',
		'collectorForClass',
		'collectorForClassVariable',
		'collectorForLiteral',
		'collectorForAll',
		'collectorForAssignment',
		'collectorForBlock'
	],
	#category : 'SmartSuggestions-Suggestion'
}

{ #category : #cleanup }
SugsSuggestionFactory class >> cleanUp [
	<script>
	collectorForInstancesVariable := nil.
	collectorForTemporaryVariable := nil.
	collectorForUndeclaredVariable := nil.
	collectorForSourceCode := nil.
	collectorForMethod := nil.
	collectorForMessage := nil.
	collectorForClass := nil.
	collectorForClassVariable := nil.
	collectorForLiteral := nil.
	collectorForAll := nil.
	collectorForAssignment := nil.
	collectorForBlock := nil.
]

{ #category : #private }
SugsSuggestionFactory class >> collectCommandsFrom: aPragmaCollector [
	| commands |
	commands := SortedCollection
		sortBlock: [ :st :nd | st position <= nd position ].
	aPragmaCollector
		do: [ :prg | 
			commands
				add: (prg methodClass theNonMetaClass perform: prg methodSelector) ].
	^ commands
]

{ #category : #private }
SugsSuggestionFactory class >> collectorForAssignment [
	^ collectorForAssignment ifNil: [collectorForAssignment := self createCollector: #assignmentCommand].

]

{ #category : #private }
SugsSuggestionFactory class >> collectorForBlock [
	^ collectorForBlock ifNil: [collectorForBlock:= self createCollector: #blockCommand].

]

{ #category : #private }
SugsSuggestionFactory class >> collectorForClass [ 
	^ collectorForClass ifNil: [collectorForClass := self createCollector: #classCommand].

]

{ #category : #private }
SugsSuggestionFactory class >> collectorForClassVariable [ 
	^ collectorForClassVariable ifNil: [collectorForClassVariable := self createCollector: #classVarCommand].

]

{ #category : #private }
SugsSuggestionFactory class >> collectorForInstancesVariable [ 
	^ collectorForInstancesVariable ifNil: [collectorForInstancesVariable := self createCollector: #instVarCommand].

]

{ #category : #private }
SugsSuggestionFactory class >> collectorForLiteral [
	^ collectorForLiteral ifNil: [collectorForLiteral := self createCollector: #literalCommand].

]

{ #category : #private }
SugsSuggestionFactory class >> collectorForMessage [
	^ collectorForMessage ifNil: [collectorForMessage := self createCollector: #messageCommand].

]

{ #category : #private }
SugsSuggestionFactory class >> collectorForMethod [ 
	^ collectorForMethod ifNil: [collectorForMethod := self createCollector: #methodCommand].

]

{ #category : #private }
SugsSuggestionFactory class >> collectorForSourceCode [ 
	^ collectorForSourceCode ifNil: [collectorForSourceCode := self createCollector: #sourceCodeCommand].

]

{ #category : #private }
SugsSuggestionFactory class >> collectorForTemporaryVariable [ 
	^ collectorForTemporaryVariable  ifNil: [collectorForTemporaryVariable := self createCollector: #tempVarCommand].
	
]

{ #category : #private }
SugsSuggestionFactory class >> collectorForUndeclaredVariable [ 
	^ collectorForUndeclaredVariable ifNil: [collectorForUndeclaredVariable := self createCollector: #undeclaredVarCommand].

]

{ #category : #literal }
SugsSuggestionFactory class >> commandsFoLiteralNode [
	^self collectCommandsFrom: self collectorForLiteral .
]

{ #category : #assignment }
SugsSuggestionFactory class >> commandsForAssignment [
	^self collectCommandsFrom: self collectorForAssignment.
]

{ #category : #block }
SugsSuggestionFactory class >> commandsForBlock [
	^self collectCommandsFrom: self collectorForBlock.
]

{ #category : #class }
SugsSuggestionFactory class >> commandsForClass [
	^self collectCommandsFrom: self collectorForClass.
]

{ #category : #class-var }
SugsSuggestionFactory class >> commandsForClassVariable [
	^self collectCommandsFrom: self collectorForClassVariable.
]

{ #category : #inst-var }
SugsSuggestionFactory class >> commandsForInstanceVariable [
	^self collectCommandsFrom: self collectorForInstancesVariable .
	

]

{ #category : #messages }
SugsSuggestionFactory class >> commandsForMessage [
	^self collectCommandsFrom: self collectorForMessage.

]

{ #category : #methods }
SugsSuggestionFactory class >> commandsForMethod [
	^self collectCommandsFrom: self collectorForMethod.
]

{ #category : #source }
SugsSuggestionFactory class >> commandsForSource [

	^self collectCommandsFrom: self collectorForSourceCode .

]

{ #category : #temp-var }
SugsSuggestionFactory class >> commandsForTemporaryVariable [
	^self collectCommandsFrom: self collectorForTemporaryVariable .
]

{ #category : #unknown-var }
SugsSuggestionFactory class >> commandsForUndeclaredVariable [
	^self collectCommandsFrom: self collectorForUndeclaredVariable  .
]

{ #category : #inst-var }
SugsSuggestionFactory class >> createAccesorsFor [
	<instVarCommand>
	^ SugsCreateInstanceVarAccessors
		icon: (self iconNamed: #smallLanguageIcon)
		position: 100
]

{ #category : #inst-var }
SugsSuggestionFactory class >> createAccessesToVariable [
	<instVarCommand>
	^ SugsAccessesToInstVar
		icon: (self iconNamed: #smallInspectItIcon)
		position: 50
]

{ #category : #class-var }
SugsSuggestionFactory class >> createAccessors [
	<classVarCommand>
	^ SugsCreateClassVarAccessors
		icon: (self iconNamed: #smallLanguageIcon)
		position: 30
]

{ #category : #methods }
SugsSuggestionFactory class >> createAddParameter [
	<methodCommand>
	^ SugsAddParameter
		icon: (self iconNamed: #smallPushpinIcon)
		position: 200
]

{ #category : #methods }
SugsSuggestionFactory class >> createBreakCommand [
	<globalCommand>
	^ SugsBreakAlwaysSuggestion
		icon: (self iconNamed: #smallCancelIcon)
		position: 15
		keyText: 'b,a'
]

{ #category : #methods }
SugsSuggestionFactory class >> createBreakConditionCommand [
	<globalCommand>
	^ SugsBreakConditionSuggestion
		icon: (self iconNamed: #smallCancelIcon)
		position: 17
		keyText: 'b,c'
]

{ #category : #methods }
SugsSuggestionFactory class >> createBreakOnceCommand [
	<globalCommand>
	^ SugsBreakOnceSuggestion
		icon: (self iconNamed: #smallCancelIcon)
		position: 16
		keyText: 'b,o'
]

{ #category : #class }
SugsSuggestionFactory class >> createBrowseReferences [
	<classCommand>
	^ SugsBrowseClassReferences
		icon: (self iconNamed: #smallInspectItIcon)
		position: 10
]

{ #category : #private }
SugsSuggestionFactory class >> createCollector: key [ 
	| collector |
	collector :=  PragmaCollector filter: [:pragma | pragma keyword=key].
	collector reset.
	collector whenChangedSend: #reset to: collector.
	^collector.

]

{ #category : #methods }
SugsSuggestionFactory class >> createCounterCommand [
	<methodCommand>
	<messageCommand>
	<assignmentCommand>
	<tempVarCommand>
	<instVarCommand>
	<classVarCommand>
	^ SugsCounterSuggestion
		icon: (self iconNamed: #classIcon)
		position: 18
]

{ #category : #methods }
SugsSuggestionFactory class >> createDeprecateMethod [
	<methodCommand>
	^ SugsDeprecateMethod
		icon: (self iconNamed: #smallLanguageIcon)
		position: 125
]

{ #category : #source }
SugsSuggestionFactory class >> createExtractMethod [
	<messageCommand>
	<literalCommand>
	<blockCommand>
	^ SugsExtractMethod
		icon: (self iconNamed: #smallUpdateIcon)
		position: 50
]

{ #category : #source }
SugsSuggestionFactory class >> createExtractSentences [
	<sourceCodeCommand>
	^ SugsExtractStatements
		icon: (self iconNamed: #smallUpdateIcon)
		position: 10
]

{ #category : #source }
SugsSuggestionFactory class >> createExtractToLocalVariable [
	<sourceCodeCommand>
	<messageCommand>
	<literalCommand>
	<blockCommand>
	^ SugsExtractToLocalVariable
		icon: (self iconNamed: #smallJumpIcon)
		position: 20
]

{ #category : #messages }
SugsSuggestionFactory class >> createFindImplementors [
	<messageCommand>
	^ SugsFindImplementors
		icon: (self iconNamed: #smallFindIcon)
		position: 11
		keyText: 'm'
]

{ #category : #messages }
SugsSuggestionFactory class >> createFindSenders [
	<methodCommand>
	<messageCommand>
	^ SugsFindSenders
		icon: (self iconNamed: #smallFindIcon)
		position: 10
		keyText: 'n'
]

{ #category : #source }
SugsSuggestionFactory class >> createFormatSourceCode [
	<methodCommand>
	<sourceCodeCommand>
	^ SugsFormatCode
		icon: (self iconNamed: #smallJustifiedIcon)
		position: 0
		keyText: 'u'
]

{ #category : #messages }
SugsSuggestionFactory class >> createInline [
	<messageCommand>
	^ SugsInline
		icon: (self iconNamed: #smallRightFlushIcon)
		position: 100
]

{ #category : #assignment }
SugsSuggestionFactory class >> createInlineTemporary [
	<assignmentCommand>
	^ SugsInlineTemporary
		icon: (self iconNamed: #smallLanguageIcon)
		position: 10
]

{ #category : #methods }
SugsSuggestionFactory class >> createMoveMethod [
	<methodCommand>
	^ SugsDelegateMethod
		icon: (self iconNamed: #smallRedoIcon)
		position: 210
]

{ #category : #methods }
SugsSuggestionFactory class >> createPullUp [
	<methodCommand>
	^ SugsPullUpMethod
		icon: (self iconNamed: #smallRedoIcon)
		position: 100
]

{ #category : #methods }
SugsSuggestionFactory class >> createPushDown [
	<methodCommand>
	^ SugsPushDownMethod
		icon: (self iconNamed: #smallUndoIcon)
		position: 101
]

{ #category : #methods }
SugsSuggestionFactory class >> createRemoveMethod [
	<methodCommand>
	^ SugsRemoveMethod
		icon: (self iconNamed: #smallCancelIcon)
		position: 20
]

{ #category : #methods }
SugsSuggestionFactory class >> createRemoveParameter [
	<methodCommand>
	^ SugsRemoveParameter
		icon: (self iconNamed: #smallCancelIcon)
		position: 210
]

{ #category : #class }
SugsSuggestionFactory class >> createRenameClass [
	<classCommand>
	^ SugsRenameClass
		icon: (self iconNamed: #smallLanguageIcon)
		position: 20
		keyText: 'r'
]

{ #category : #class-var }
SugsSuggestionFactory class >> createRenameClassVariable [
	<classVarCommand>
	^ SugsRenameClassVar
		icon: (self iconNamed: #smallLanguageIcon)
		position: 10
		keyText: 'r'
]

{ #category : #methods }
SugsSuggestionFactory class >> createRenameMethod [
	<methodCommand>
	^ SugsRenameMethod
		icon: (self iconNamed: #smallLanguageIcon)
		position: 10
		keyText: 'r,m'
]

{ #category : #temp-var }
SugsSuggestionFactory class >> createRenameTemporaryVariable [
	<tempVarCommand>
	^ SugsRenameTemporaryVar
		icon: (self iconNamed: #smallLanguageIcon)
		position: 10
		keyText: 'r'
]

{ #category : #inst-var }
SugsSuggestionFactory class >> createRenameVariable [
	<instVarCommand>
	^ SugsRenameInstanceVar
		icon: (self iconNamed: #smallLanguageIcon)
		position: 10
		keyText: 'r'
]

{ #category : #methods }
SugsSuggestionFactory class >> createSwapMethod [
	<methodCommand>
	^ SugsSwapMethod new
		icon: (self iconNamed: #smallRedoIcon);
		position: 50
]

{ #category : #class-var }
SugsSuggestionFactory class >> createUseAccessorToVar [
	<classVarCommand>
	^ SugsUseAccessorsForClassVar
		icon: (self iconNamed: #smallLanguageIcon)
		position: 10
]

{ #category : #unknown-var }
SugsSuggestionFactory class >> createVariableCorrection [
	"	<undeclaredVarCommand>"

	"Unable because it's not fully implemented"

	| variables index |
	^ SugsSuggestion
		for: [ :context | 
			variables := context selectedClass allInstVarNames asArray sort.
			index := UIManager default chooseFrom: variables.
			UIManager default inform: (variables at: index) ]
		named: 'Correct variable to...' translated
		icon: (self iconNamed: #smallFindIcon)
]

{ #category : #inst-var }
SugsSuggestionFactory class >> createVariableStoresInto [
	<instVarCommand>
	^ SugsStoresIntoInstVar
		icon: (self iconNamed: #smallInspectItIcon)
		position: 60
]

{ #category : #methods }
SugsSuggestionFactory class >> createWatchpointCommand [
	<methodCommand>
	<messageCommand>
	<assignmentCommand>
	<tempVarCommand>
	<instVarCommand>
	<classVarCommand>
	^ SugsWatchpointSuggestion
		icon: (self iconNamed: #haloView)
		position: 18
]

{ #category : #private }
SugsSuggestionFactory class >> globalCollector [
	^ collectorForAll ifNil: [collectorForAll  := self createCollector: #globalCommand].

]

{ #category : #global }
SugsSuggestionFactory class >> globalCommands [
	^self collectCommandsFrom: self globalCollector.
]
