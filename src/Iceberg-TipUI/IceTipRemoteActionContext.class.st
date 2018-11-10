"
I'm a base context for remote action contexts (pull, push)
"
Class {
	#name : #IceTipRemoteActionContext,
	#superclass : #IceTipContext,
	#instVars : [
		'repositoryDetached'
	],
	#category : #'Iceberg-TipUI-Commands'
}

{ #category : #initialization }
IceTipRemoteActionContext >> initialize [
	super initialize.
	repositoryDetached := false
]

{ #category : #testing }
IceTipRemoteActionContext >> isRepositoryDetached [
	^ repositoryDetached
]

{ #category : #accessing }
IceTipRemoteActionContext >> readContextParametersFromModel: aModel [
	super readContextParametersFromModel: aModel.
	repositoryDetached := aModel isRepositoryDetached
]
