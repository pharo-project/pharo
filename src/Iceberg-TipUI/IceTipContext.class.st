"
I'm a generic context for iceberg tools.
"
Class {
	#name : #IceTipContext,
	#superclass : #CmdToolContext,
	#instVars : [
		'repositoryModel'
	],
	#category : #'Iceberg-TipUI-Commands'
}

{ #category : #testing }
IceTipContext >> hasUnbornProject [
	^ self repositoryModel hasUnbornProject
]

{ #category : #testing }
IceTipContext >> isRepositoryDetached [
	^ self repositoryModel isNil 
		or: [ self  repositoryModel isDetached ]
]

{ #category : #testing }
IceTipContext >> isRepositoryMissing [
	^ self repositoryModel isMissing or: [ self repositoryModel isCodeMissing ]
]

{ #category : #testing }
IceTipContext >> isRepositoryOperational [
	^ self isRepositoryMissing not 
		and: [ self isRepositoryDetached not
			and: [ self hasUnbornProject not ] ]
]

{ #category : #accessing }
IceTipContext >> readContextParametersFromModel: aModel [
	self repositoryModel: aModel repositoryModel
]

{ #category : #accessing }
IceTipContext >> repositoryModel [
	^ repositoryModel
]

{ #category : #accessing }
IceTipContext >> repositoryModel: anObject [
	repositoryModel := anObject
]

{ #category : #accessing }
IceTipContext >> toolAnnouncer [
	^ self tool announcer
]
