"
An AbstractMessageCommand is command that creates ``@object metavariable.


"
Class {
	#name : #AbstractMessageCommand,
	#superclass : #AbstractCommand,
	#category : #'RewriteTool-Command'
}

{ #category : #accessing }
AbstractMessageCommand >> abstractName [
	^ '``@object'
]

{ #category : #execute }
AbstractMessageCommand >> execute [
	^ self abstractWithName: self abstractName andIndex: self panel varIndex
]
