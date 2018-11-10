"
I'm a model to present pull commits and operations.
"
Class {
	#name : #IceTipPullModel,
	#superclass : #IceTipRemoteActionModel,
	#category : #'Iceberg-TipUI-Model'
}

{ #category : #accessing }
IceTipPullModel >> commitModels [
	^ (self entity branch incomingCommitsFrom: self remote)
		collect: [ :each | 
			(IceTipCommitModel 
				repositoryModel: self repositoryModel 
				on: each) 
				beCached ]
]

{ #category : #actions }
IceTipPullModel >> pullThen: aBlock [
	IceTipStandardAction new 
		repository: self entity;
		message: ('Pulling from {1}' format: { self remote name });
		onSuccessRepositoryModified;
		onSuccess: aBlock;
		execute: [ self entity pullFrom: self remote ]
]
