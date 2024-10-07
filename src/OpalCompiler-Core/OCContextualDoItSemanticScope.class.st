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
	#name : 'OCContextualDoItSemanticScope',
	#superclass : 'OCDoItSemanticScope',
	#instVars : [
		'targetContext',
		'importedVariables'
	],
	#category : 'OpalCompiler-Core-Semantics',
	#package : 'OpalCompiler-Core',
	#tag : 'Semantics'
}

{ #category : 'instance creation' }
OCContextualDoItSemanticScope class >> targetingContext: aContext [
	^self new
		targetContext: aContext
]

{ #category : 'code evaluation' }
OCContextualDoItSemanticScope >> announceDoItEvaluation: arg1 by: arg2 [

	arg2 evaluated: arg1 context: targetContext
]

{ #category : 'testing' }
OCContextualDoItSemanticScope >> hasBindingThatBeginsWith: arg1 [

	(targetContext astScope hasBindingThatBeginsWith: arg1) ifTrue: [
		^ true ].
	nil.
	^ super hasBindingThatBeginsWith: arg1
]

{ #category : 'lookup' }
OCContextualDoItSemanticScope >> importVariable: arg1 [

	^ importedVariables
		  at: arg1 name
		  ifAbsentPut: [ arg1 asDoItVariableFrom: targetContext ]
]

{ #category : 'accessing' }
OCContextualDoItSemanticScope >> importedVariables [

	^ importedVariables
]

{ #category : 'accessing' }
OCContextualDoItSemanticScope >> importedVariables: arg1 [

	importedVariables := arg1
]

{ #category : 'initialization' }
OCContextualDoItSemanticScope >> initialize [

	super initialize.
	importedVariables := Dictionary new
]

{ #category : 'lookup' }
OCContextualDoItSemanticScope >> lookupVar: arg1 [

	| tmp1 tmp3 |
	tmp3 := tmp1 := targetContext lookupVar: arg1.
	tmp3 ifNotNil: [ ^ self importVariable: tmp1 ].
	^ super lookupVar: arg1
]

{ #category : 'accessing' }
OCContextualDoItSemanticScope >> receiver [

	^ targetContext receiver
]

{ #category : 'accessing' }
OCContextualDoItSemanticScope >> targetContext [

	^ targetContext
]

{ #category : 'accessing' }
OCContextualDoItSemanticScope >> targetContext: arg1 [

	targetContext := arg1
]
