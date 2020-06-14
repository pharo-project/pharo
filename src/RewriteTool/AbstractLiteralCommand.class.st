"
An AbstractLiteralCommand is command that creates `#literal metavariable.

"
Class {
	#name : #AbstractLiteralCommand,
	#superclass : #AbstractCommand,
	#category : #'RewriteTool-Command'
}

{ #category : #accessing }
AbstractLiteralCommand >> abstractName [
	^ '`#literal'
]

{ #category : #execute }
AbstractLiteralCommand >> execute [
	^ self abstractWithName: self abstractName andIndex: self panel literalIndex
]
