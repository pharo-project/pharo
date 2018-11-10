"
I'm a base model for all commitish models to be used.
I can provide #commitModels which changes depending on concrete model.
"
Class {
	#name : #IceTipCommitishModel,
	#superclass : #IceTipEntityModel,
	#category : #'Iceberg-TipUI-Model'
}

{ #category : #accessing }
IceTipCommitishModel >> commitId [
	
	^ self entity commit id
]

{ #category : #accessing }
IceTipCommitishModel >> commitModels [
	self subclassResponsibility
]

{ #category : #accessing }
IceTipCommitishModel >> shortCommitId [
	
	^ self entity shortId
]
