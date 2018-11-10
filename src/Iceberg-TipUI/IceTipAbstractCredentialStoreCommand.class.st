"
I am the abstract class of all the general commands in the credentials list.
"
Class {
	#name : #IceTipAbstractCredentialStoreCommand,
	#superclass : #IceTipCommand,
	#instVars : [
		'store',
		'tool'
	],
	#category : #'Iceberg-TipUI-Credentials'
}

{ #category : #execution }
IceTipAbstractCredentialStoreCommand >> readParametersFromContext: aToolContext [
	
	store := aToolContext store.
	tool := aToolContext tool
]
