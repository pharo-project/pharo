"
I am an inspector showing abstract syntax trees. 
Instead of showing the full complexity, show the structure of the code by only showing the nodes.

As an example, inspect 

	(Object>>#halt) ast

"
Class {
	#name : #SpEyeSyntaxTreeInspector,
	#superclass : #SpEyeTreeInspector,
	#category : #'Spec2-Inspector-Base'
}

{ #category : #accessing }
SpEyeSyntaxTreeInspector class >> label [
	^ 'AST Inspector'
]

{ #category : #api }
SpEyeSyntaxTreeInspector >> childrenForObject: aNode [
	^ aNode children collect: [ :each | 
			SpDynamicEyeElement
				host: aNode
				label: each class name
				description: [ :node | node formattedCode ]
				value: [each] ]
]

{ #category : #api }
SpEyeSyntaxTreeInspector >> roots [
	^ self childrenForObject: self object
]
