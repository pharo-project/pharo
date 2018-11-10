"
I'm a the abstract superclass of commands that act on repositories. 
"
Class {
	#name : #IceTipRepositoryCommand,
	#superclass : #IceTipCommand,
	#instVars : [
		'announcer'
	],
	#category : #'Iceberg-TipUI-Commands'
}

{ #category : #testing }
IceTipRepositoryCommand class >> actsOnBranch: aBranchModel [
	^ false
]

{ #category : #testing }
IceTipRepositoryCommand class >> actsOnRemote: aRemoteModel [
	^ false
]

{ #category : #testing }
IceTipRepositoryCommand class >> actsOnTag: aTagModel [
	^ false
]

{ #category : #accessing }
IceTipRepositoryCommand class >> isAbstract [

	^ self == IceTipRepositoryCommand
]

{ #category : #accessing }
IceTipRepositoryCommand >> announcer [
	^ announcer ifNotNil: #contents
]

{ #category : #execution }
IceTipRepositoryCommand >> readParametersFromContext: aToolContext [
	super readParametersFromContext: aToolContext.
	announcer := IceTipWeakValueHolder contents: aToolContext toolAnnouncer.
]
