"
I am a root scope for any DoIt expression evaluated in given context (doIt in the debugger).

I extend the variable visibility to allow the usage of targetContext variables. 
In debugger it means that users can execute or debug the code containing local variables. They are indirectly accessible from the expression doIt methods.

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
OCContextualDoItSemanticScope >> asDoItForReceiver: anObject [ 
	anObject == targetContext receiver ifFalse: [ 
		self error: 'Receiver is not same as context receiver' ].
	
	^self
]

{ #category : #'code compilation' }
OCContextualDoItSemanticScope >> buildASTBy: aParser [

	| ast |
	ast := aParser parseExpression.
	ast := ast asDoitForContext: targetContext.
	ast semanticScope: self.
	^ast
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
	^expressionAST asDoitForContext: targetContext
]
