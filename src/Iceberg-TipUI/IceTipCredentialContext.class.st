"
I am the context used when working on a credential. 

"
Class {
	#name : #IceTipCredentialContext,
	#superclass : #CmdToolContext,
	#instVars : [
		'credential',
		'store'
	],
	#category : #'Iceberg-TipUI-Credentials'
}

{ #category : #accessing }
IceTipCredentialContext >> credential [
	^ credential
]

{ #category : #accessing }
IceTipCredentialContext >> credential: anObject [
	credential := anObject
]

{ #category : #accessing }
IceTipCredentialContext >> item [
	"I return an entity model, as it is expected by the users."
	^ IceTipEntityModel on: credential
]

{ #category : #accessing }
IceTipCredentialContext >> repositoryModel [
	^ nil
]

{ #category : #accessing }
IceTipCredentialContext >> store [
	^ store
]

{ #category : #accessing }
IceTipCredentialContext >> store: anObject [
	store := anObject
]
