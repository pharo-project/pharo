"
Annotations are message send witout receiver and starting with `@`.

```st
@something
```

In order to keep a simple and powerful parser, annotations are implemented as classic message sends on a special receiver `@`. The current class models this receiver.
AST validations and transformations will deal with this.

Note: it's not a variable, merly a placeholder.
"
Class {
	#name : #RBAnnotationMarkNode,
	#superclass : #RBValueNode,
	#instVars : [
		'start'
	],
	#category : #'AST-Core-Nodes'
}

{ #category : #visiting }
RBAnnotationMarkNode >> acceptVisitor: aProgramNodeVisitor [
	^ aProgramNodeVisitor visitAnnotationMarkNode: self
]

{ #category : #initialization }
RBAnnotationMarkNode >> initialize [
	super initialize.
	start := 0
]

{ #category : #testing }
RBAnnotationMarkNode >> isAnnotationMark [
	^ true
]

{ #category : #testing }
RBAnnotationMarkNode >> needsParenthesis [
	^false
]

{ #category : #accessing }
RBAnnotationMarkNode >> start: aPosition [
	"Beware, start is in fact `startWithoutParentheses` as in RBValueNode, start includes parentheses"

	start := aPosition
]

{ #category : #accessing }
RBAnnotationMarkNode >> startWithoutParentheses [
	^ start
]

{ #category : #accessing }
RBAnnotationMarkNode >> stopWithoutParentheses [
	^ start + 1
]
