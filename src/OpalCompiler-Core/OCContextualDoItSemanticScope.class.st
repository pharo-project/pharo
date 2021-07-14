"
I am a scope for DoIt expressions targeting a given context (doIt/printIt in the debugger).

I implement a visibility of varibles defined in the target context:
- temps of context 
- inst vars of context receiver
- etc
Thus the doIt expressions can transparently access ""external"" variables.
In debugger it means that users can execute or debug the code containing local variables. 

Internally I convert all external variables using #asDoItVariable message and cache them in #importedVariables dictionary. This way I ensure the only instance of DoItVariable per each external variable (the variable lookup can happens multiple times for same name).  

My instances can be created using following expression:

	OCContextualDoItSemanticScope targetingContext: thisContext 

"
Class {
	#name : #OCContextualDoItSemanticScope,
	#superclass : #OCDoItSemanticScope,
	#instVars : [
		'targetContext',
		'importedVariables'
	],
	#category : #'OpalCompiler-Core-Semantics'
}

{ #category : #'instance creation' }
OCContextualDoItSemanticScope class >> targetingContext: aContext [ 
	^self new 
		targetContext: aContext
]

{ #category : #'code evaluation' }
OCContextualDoItSemanticScope >> announceDoItEvaluation: aSourceString by: aSystemAnnouncer [ 

	aSystemAnnouncer evaluated: aSourceString context: targetContext
]

{ #category : #converting }
OCContextualDoItSemanticScope >> asDoItScopeForReceiver: anObject [ 
	anObject == targetContext receiver ifFalse: [ 
		self error: 'Receiver is not same as context receiver' ].
	
	^self
]

{ #category : #'code evaluation' }
OCContextualDoItSemanticScope >> evaluateDoIt: doItMethod [

	^targetContext receiver withArgs: { targetContext } executeMethod: doItMethod
]

{ #category : #testing }
OCContextualDoItSemanticScope >> hasBindingThatBeginsWith: aString [
	
	(targetContext astScope hasBindingThatBeginsWith: aString) ifTrue: [ ^true ].
	
	^super hasBindingThatBeginsWith: aString

]

{ #category : #lookup }
OCContextualDoItSemanticScope >> importVariable: aVariable [ 

	^importedVariables 
		at: aVariable name 
		ifAbsentPut: [ aVariable asDoItVariableFrom: targetContext ]
]

{ #category : #accessing }
OCContextualDoItSemanticScope >> importedVariables [

	^ importedVariables
]

{ #category : #accessing }
OCContextualDoItSemanticScope >> importedVariables: anObject [

	importedVariables := anObject
]

{ #category : #initialization }
OCContextualDoItSemanticScope >> initialize [ 
	super initialize.
	
	importedVariables := Dictionary new 
]

{ #category : #lookup }
OCContextualDoItSemanticScope >> lookupVar: name declare: aBoolean [

	(targetContext lookupVar: name declare: aBoolean) ifNotNil: [ :v | ^self importVariable: v].

	^super lookupVar: name declare: aBoolean
]

{ #category : #accessing }
OCContextualDoItSemanticScope >> targetClass [ 
	^targetContext compiledCode methodClass
]

{ #category : #accessing }
OCContextualDoItSemanticScope >> targetContext [

	^ targetContext
]

{ #category : #accessing }
OCContextualDoItSemanticScope >> targetContext: aContext [

	targetContext := aContext
]

{ #category : #'code compilation' }
OCContextualDoItSemanticScope >> transformDoItExpressionToMethodAST: expressionAST [
	^expressionAST asDoItForContext: targetContext
]
