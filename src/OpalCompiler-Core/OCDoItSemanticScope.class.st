"
I am a root of hierarchy of scopes representing a various kinds of of DoIt expressions.

My subclasses must implement several methods for doIt method compilation and evaluation:

- transformDoItExpressionToMethodAST: anAST

- evaluateDoIt: aMethod

See subclasses for examples
"
Class {
	#name : #OCDoItSemanticScope,
	#superclass : #OCSemanticScope,
	#category : #'OpalCompiler-Core-Semantics'
}

{ #category : #testing }
OCDoItSemanticScope class >> isAbstract [ 
	^self = OCDoItSemanticScope 
]

{ #category : #converting }
OCDoItSemanticScope >> asDoItScope [
	^self
]

{ #category : #'code compilation' }
OCDoItSemanticScope >> compileMethodFromASTBy: aCompiler [
	^aCompiler compileDoItFromAST
]

{ #category : #'code evaluation' }
OCDoItSemanticScope >> evaluateDoIt: doItMethod [
	self subclassResponsibility 
]

{ #category : #testing }
OCDoItSemanticScope >> isDoItScope [ 
	^true
]

{ #category : #parsing }
OCDoItSemanticScope >> parseASTBy: aParser [
	^aParser parseExpression
]

{ #category : #'code compilation' }
OCDoItSemanticScope >> transformDoItExpressionToMethodAST: expressionAST [
	self subclassResponsibility 
]
