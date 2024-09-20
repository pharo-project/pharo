"
Annotations are message send witout receiver and starting with `@`.

E.g.
```st
@foo.
@foo: 5 bar: self something.
foo := @bar
```

In order to keep a simple and powerful parser, annotations are implemented as classic message sends on a special receiver `@`. The current class models this receiver.

AST validations and transformations (and most likely compiler plugins) have the responsibility to handle the annotations:

* checking the correct syntaxic and semantic usage (parsePlugin);
* transforming the AST;
* and/or attaching a custom IR emiter (see `emitValue:` and `emitValueBlock`).

Note: the node is not a value nor a variable, it is a placeholder at best.
"
Class {
	#name : 'ASTAnnotationMarkNode',
	#superclass : 'ASTValueNode',
	#instVars : [
		'start',
		'emitValueBlock'
	],
	#category : 'AST-Core-Nodes',
	#package : 'AST-Core',
	#tag : 'Nodes'
}

{ #category : 'comparing' }
ASTAnnotationMarkNode >> = anObject [
	"All annotation marks are equivalent"
	^self class = anObject class
]

{ #category : 'visiting' }
ASTAnnotationMarkNode >> acceptVisitor: aProgramNodeVisitor [
	^ aProgramNodeVisitor visitAnnotationMarkNode: self
]

{ #category : 'comparing' }
ASTAnnotationMarkNode >> hash [

	^ self class hash
]

{ #category : 'initialization' }
ASTAnnotationMarkNode >> initialize [
	super initialize.
	start := 0
]

{ #category : 'testing' }
ASTAnnotationMarkNode >> isAnnotationMark [
	^ true
]

{ #category : 'testing' }
ASTAnnotationMarkNode >> isHandled [

	^ self emitValueBlock isNotNil
]

{ #category : 'testing' }
ASTAnnotationMarkNode >> needsParenthesis [
	^false
]

{ #category : 'accessing' }
ASTAnnotationMarkNode >> precedence [
	^0
]

{ #category : 'accessing' }
ASTAnnotationMarkNode >> start: aPosition [
	"Beware, start is in fact `startWithoutParentheses` as in ASTValueNode, start includes parentheses"

	start := aPosition
]

{ #category : 'accessing' }
ASTAnnotationMarkNode >> startWithoutParentheses [
	^ start
]

{ #category : 'accessing' }
ASTAnnotationMarkNode >> stopWithoutParentheses [
	^ start
]
