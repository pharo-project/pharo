"
I am an inspector showing abstract syntax trees. 
Instead of showing the full complexity, show the structure of the code by only showing the nodes.

As an example, inspect 

	(Object>>#halt) ast

"
Class {
	#name : #EyeSyntaxTreeInspector,
	#superclass : #EyeTreeInspector,
	#category : #'Spec2-Inspector-Base'
}

{ #category : #accessing }
EyeSyntaxTreeInspector class >> label [
	^ 'AST Inspector'
]

{ #category : #api }
EyeSyntaxTreeInspector >> childrenForObject: aNode [
	^ aNode children collect: [ :each | 
			DynamicEyeElement
				host: aNode
				label: each class name
				description: [ :node | node formattedCode ]
				value: [each] ]
]

{ #category : #api }
EyeSyntaxTreeInspector >> roots [
	^ self childrenForObject: self object
]
