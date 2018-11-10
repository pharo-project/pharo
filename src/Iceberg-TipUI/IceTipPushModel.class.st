"
I'm a model to present push commits and operations.
"
Class {
	#name : #IceTipPushModel,
	#superclass : #IceTipRemoteActionModel,
	#category : #'Iceberg-TipUI-Model'
}

{ #category : #accessing }
IceTipPushModel >> commitModels [
	^ (self entity outgoingCommitsTo: self remote)
		collect: [ :each | 
			(IceTipCommitModel 
				repositoryModel: self repositoryModel 
				on: each) 
				beCached ]
]

{ #category : #actions }
IceTipPushModel >> pushThen: aBlock [
	IceTipStandardAction new 
		repository: self entity;
		message: ('Pushing to {1}' format: { self remote name }) ;
		onSuccessRepositoryModified;
		onSuccess: aBlock;
		execute: [ self entity pushTo: self remote ]
]
