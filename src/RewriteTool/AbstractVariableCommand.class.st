"
An AbstractVariableCommand is command that creates `variable metavariable.

"
Class {
	#name : #AbstractVariableCommand,
	#superclass : #AbstractCommand,
	#category : #'RewriteTool-Command'
}

{ #category : #testing }
AbstractVariableCommand class >> isAccepted: aNode [
	^ aNode isInTemporariesList not
]

{ #category : #accessing }
AbstractVariableCommand >> abstractName [
	^ '``@variable'
]

{ #category : #execute }
AbstractVariableCommand >> execute [
	^ self abstractWithName: self abstractName andIndex: self panel varIndex
]
