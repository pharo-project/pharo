"
I'm a command to perform a pull from a remote.
"
Class {
	#name : #IcePullRemoteCommand,
	#superclass : #IceRepositoryCommand,
	#instVars : [
		'remote'
	],
	#category : #'Iceberg-TipUI-Model'
}

{ #category : #execute }
IcePullRemoteCommand >> execute [
	self repository branch pullFrom: self remote.
	self beSuccess
]

{ #category : #accessing }
IcePullRemoteCommand >> remote [
	^ remote
]

{ #category : #accessing }
IcePullRemoteCommand >> remote: anObject [
	remote := anObject
]
