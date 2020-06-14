"
An AbstractBlockCommand is command that creates `@block metavariable.

"
Class {
	#name : #AbstractBlockCommand,
	#superclass : #AbstractCommand,
	#category : #'RewriteTool-Command'
}

{ #category : #accessing }
AbstractBlockCommand >> abstractName [
	^ '`@block'
]

{ #category : #execute }
AbstractBlockCommand >> execute [
	^ self abstractWithName: self abstractName andIndex: self panel blockIndex
]
