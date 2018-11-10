"
I'm a model for IceRemote entries.
"
Class {
	#name : #IceTipRemoteModel,
	#superclass : #IceTipEntityModel,
	#traits : 'TWithBranchModel',
	#classTraits : 'TWithBranchModel classTrait',
	#category : #'Iceberg-TipUI-Model'
}

{ #category : #accessing }
IceTipRemoteModel >> branches [
	^ self entity branches
]

{ #category : #actions }
IceTipRemoteModel >> delete [
	<noCache>
	
	IceTipStandardAction new
		repository: self repositoryModel entity;
		message: ('Removing remote {1}' format: { self name });
		onSuccessRepositoryModified;
		do: [ self repositoryModel entity removeRemote: self entity ]
]

{ #category : #accessing }
IceTipRemoteModel >> descriptionWithDecoration [
	^ '{1} <{2}>' format: { self name. self entity url }
]

{ #category : #actions }
IceTipRemoteModel >> fetch [
	<noCache>
	
	IceTipStandardAction new
		repository: self repositoryModel entity;
		message: ('Fetching remote {1}' format: { self name });
		onSuccessRepositoryModified;
		do: [ self entity fetch ]
]

{ #category : #accessing }
IceTipRemoteModel >> name [
	^ self entity name
]
