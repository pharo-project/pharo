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
	#category : #'SmartSuggestions-Suggestion'
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
				add: (prg methodClass instanceSide perform: prg methodSelector) ].
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

{ #category : #'class-var' }
SugsSuggestionFactory class >> commandsForClassVariable [
	^self collectCommandsFrom: self collectorForClassVariable.
]

{ #category : #'inst-var' }
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

{ #category : #'temp-var' }
SugsSuggestionFactory class >> commandsForTemporaryVariable [
	^self collectCommandsFrom: self collectorForTemporaryVariable .
]

{ #category : #'inst-var' }
SugsSuggestionFactory class >> createAccesorsFor [
	<instVarCommand>
	^ SugsCreateInstanceVarAccessors new
]

{ #category : #'inst-var' }
SugsSuggestionFactory class >> createAccessesToVariable [
	<instVarCommand>
	^ SugsAccessesToInstVar new
]

{ #category : #'class-var' }
SugsSuggestionFactory class >> createAccessors [
	<classVarCommand>
	^ SugsCreateClassVarAccessors new
]

{ #category : #methods }
SugsSuggestionFactory class >> createAddParameter [
	<methodCommand>
	^ SugsAddParameter new
]

{ #category : #methods }
SugsSuggestionFactory class >> createBreakCommand [
	<globalCommand>
	^ SugsBreakAlwaysSuggestion new
]

{ #category : #methods }
SugsSuggestionFactory class >> createBreakConditionCommand [
	<globalCommand>
	^ SugsBreakConditionSuggestion new
]

{ #category : #methods }
SugsSuggestionFactory class >> createBreakOnceCommand [
	<globalCommand>
	^ SugsBreakOnceSuggestion new
]

{ #category : #class }
SugsSuggestionFactory class >> createBrowseReferences [
	<classCommand>
	^ SugsBrowseClassReferences new
]

{ #category : #class }
SugsSuggestionFactory class >> createBrowseTraitUsers [
	<classCommand>
	^ SugsBrowseTraitUsers new
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
	^ SugsCounterSuggestion new
]

{ #category : #methods }
SugsSuggestionFactory class >> createDeprecateMethod [
	<methodCommand>
	^ SugsDeprecateMethod new
]

{ #category : #source }
SugsSuggestionFactory class >> createExtractMethod [
	<messageCommand>
	<literalCommand>
	<blockCommand>
	^ SugsExtractMethod new
]

{ #category : #source }
SugsSuggestionFactory class >> createExtractSentences [
	<sourceCodeCommand>
	^ SugsExtractStatements new
]

{ #category : #source }
SugsSuggestionFactory class >> createExtractToLocalVariable [
	<sourceCodeCommand>
	<messageCommand>
	<literalCommand>
	<blockCommand>
	^ SugsExtractToLocalVariable new
]

{ #category : #messages }
SugsSuggestionFactory class >> createFindImplementors [
	<messageCommand>
	^ SugsFindImplementors new
]

{ #category : #messages }
SugsSuggestionFactory class >> createFindSenders [
	<methodCommand>
	<messageCommand>
	^ SugsFindSenders new
]

{ #category : #source }
SugsSuggestionFactory class >> createFormatSourceCode [
	<methodCommand>
	<sourceCodeCommand>
	^ SugsFormatCode new
]

{ #category : #messages }
SugsSuggestionFactory class >> createInline [
	<messageCommand>
	^ SugsInline new
]

{ #category : #assignment }
SugsSuggestionFactory class >> createInlineTemporary [
	<assignmentCommand>
	^ SugsInlineTemporary new
]

{ #category : #methods }
SugsSuggestionFactory class >> createMoveMethod [
	<methodCommand>
	^ SugsDelegateMethod new
]

{ #category : #methods }
SugsSuggestionFactory class >> createPullUp [
	<methodCommand>
	^ SugsPullUpMethod new
]

{ #category : #methods }
SugsSuggestionFactory class >> createPushDown [
	<methodCommand>
	^ SugsPushDownMethod new
]

{ #category : #methods }
SugsSuggestionFactory class >> createRemoveMethod [
	<methodCommand>
	^ SugsRemoveMethod new
]

{ #category : #methods }
SugsSuggestionFactory class >> createRemoveParameter [
	<methodCommand>
	^ SugsRemoveParameter new
]

{ #category : #class }
SugsSuggestionFactory class >> createRenameClass [
	<classCommand>
	^ SugsRenameClass new
]

{ #category : #'class-var' }
SugsSuggestionFactory class >> createRenameClassVariable [
	<classVarCommand>
	^ SugsRenameClassVar new
]

{ #category : #methods }
SugsSuggestionFactory class >> createRenameMethod [
	<methodCommand>
	^ SugsRenameMethod new
]

{ #category : #'temp-var' }
SugsSuggestionFactory class >> createRenameTemporaryVariable [
	<tempVarCommand>
	^ SugsRenameTemporaryVar new
]

{ #category : #'inst-var' }
SugsSuggestionFactory class >> createRenameVariable [
	<instVarCommand>
	^ SugsRenameInstanceVar new
]

{ #category : #methods }
SugsSuggestionFactory class >> createSwapMethod [
	<methodCommand>
	^ SugsSwapMethod new
]

{ #category : #'class-var' }
SugsSuggestionFactory class >> createUseAccessorToVar [
	<classVarCommand>
	^ SugsUseAccessorsForClassVar new
]

{ #category : #'inst-var' }
SugsSuggestionFactory class >> createVariableStoresInto [
	<instVarCommand>
	^ SugsStoresIntoInstVar new
]

{ #category : #methods }
SugsSuggestionFactory class >> createWatchpointCommand [
	<methodCommand>
	<messageCommand>
	<assignmentCommand>
	<tempVarCommand>
	<instVarCommand>
	<classVarCommand>
	^ SugsWatchpointSuggestion new
]

{ #category : #private }
SugsSuggestionFactory class >> globalCollector [
	^ collectorForAll ifNil: [collectorForAll  := self createCollector: #globalCommand].

]

{ #category : #global }
SugsSuggestionFactory class >> globalCommands [
	^self collectCommandsFrom: self globalCollector.
]
