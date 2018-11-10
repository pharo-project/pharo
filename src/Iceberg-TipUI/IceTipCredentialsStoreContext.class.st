"
I am the context when working on a credentials store.
"
Class {
	#name : #IceTipCredentialsStoreContext,
	#superclass : #CmdToolContext,
	#instVars : [
		'store'
	],
	#category : #'Iceberg-TipUI-Credentials'
}

{ #category : #accessing }
IceTipCredentialsStoreContext >> readContextParametersFromModel: aStore [ 

	store := aStore
]

{ #category : #accessing }
IceTipCredentialsStoreContext >> store [
	^ store
]
