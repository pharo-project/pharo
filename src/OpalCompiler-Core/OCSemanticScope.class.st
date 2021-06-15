"
I am an abstract class. My subclasses represent the meaning of the code: is it a method or a doIt expression.

The source code is parsed or compiled using configured semantic scope, one of my subclasses. The actual form of a final method AST or a compiled method depends on the scope: 
- doIt expressions should be converted to the doIt method
- method definition should be just parsed as a method
Subclasses define this logic using #transformDoItExpressionToMethodAST: method. 

Another responsibility of subclasses is to define the visibility of variables for the code by implementing #targetClass method. 
Target class is used for the variable lookup logic: all instance variables and shared variables are visible for the code compiled for the target class.

To be able evaluate given source code as expression the scope implements #evaluateDoIt: method


"
Class {
	#name : #OCSemanticScope,
	#superclass : #OCAbstractScope,
	#category : #'OpalCompiler-Core-Semantics'
}

{ #category : #testing }
OCSemanticScope class >> isAbstract [ 
	^self = OCSemanticScope 
]

{ #category : #accessing }
OCSemanticScope >> allTemps [
	^#()
]

{ #category : #'code evaluation' }
OCSemanticScope >> announceDoItEvaluation: aSourceString by: aSystemAnnouncer [ 

	aSystemAnnouncer evaluated: aSourceString context: nil
]

{ #category : #converting }
OCSemanticScope >> asDoItForContext: aContext [ 
	^OCContextualDoItSemanticScope targetingContext: aContext
]

{ #category : #converting }
OCSemanticScope >> asDoItForReceiver: anObject [
	^OCReceiverDoItSemanticScope targetingReceiver: anObject
]

{ #category : #converting }
OCSemanticScope >> asDoItScope [
	self subclassResponsibility 
]

{ #category : #'code compilation' }
OCSemanticScope >> buildASTBy: aParser [

	self subclassResponsibility 
]

{ #category : #'code evaluation' }
OCSemanticScope >> evaluateDoIt: doItMethod [
	self subclassResponsibility 
]

{ #category : #testing }
OCSemanticScope >> hasBindingThatBeginsWith: aString [
	
	(self targetClass hasBindingThatBeginsWith: aString) ifTrue: [ ^true ].
	
	^super hasBindingThatBeginsWith: aString

]

{ #category : #testing }
OCSemanticScope >> isDoItScope [ 
	^false
]

{ #category : #lookup }
OCSemanticScope >> lookupVar: name declare: aBoolean [

	(self targetClass lookupVar: name declare: aBoolean) ifNotNil: [ :v | ^v ].
	
	^outerScope ifNotNil: [ outerScope lookupVar: name declare: aBoolean ]
	
]

{ #category : #accessing }
OCSemanticScope >> targetClass [ 
	self subclassResponsibility  
]

{ #category : #accessing }
OCSemanticScope >> targetEnvironment [ 
	^self targetClass environment 
]

{ #category : #'code compilation' }
OCSemanticScope >> transformDoItExpressionToMethodAST: expressionAST [
	self subclassResponsibility 
]
