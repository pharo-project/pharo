"
I'm a model to provide access to all repositories registered.
"
Class {
	#name : #IceTipRepositoriesModel,
	#superclass : #IceTipModel,
	#instVars : [
		'projects'
	],
	#category : #'Iceberg-TipUI-Model'
}

{ #category : #accessing }
IceTipRepositoriesModel >> repositories [
	^ IceRepository registry collect: [ :each | (IceTipRepositoryModel on: each) beCached ]
]
