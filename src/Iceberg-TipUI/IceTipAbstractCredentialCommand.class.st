"
I am the common class of all the commands over an specific credential.
"
Class {
	#name : #IceTipAbstractCredentialCommand,
	#superclass : #IceTipCommand,
	#instVars : [
		'credential',
		'store',
		'tool'
	],
	#category : #'Iceberg-TipUI-Credentials'
}

{ #category : #execution }
IceTipAbstractCredentialCommand >> readParametersFromContext: aToolContext [
	
	credential := aToolContext credential.
	store := aToolContext store.
	tool := aToolContext tool
]
