"
I'm a model to provide the history of a method.
"
Class {
	#name : #IceTipVersionHistoryModel,
	#superclass : #IceTipCommitishModel,
	#instVars : [
		'commitModels'
	],
	#category : #'Iceberg-TipUI-Model'
}

{ #category : #accessing }
IceTipVersionHistoryModel >> commitModelAfter: aModel [
	^ self commitModels 
		after: aModel
		ifAbsent: [ nil ]
]

{ #category : #accessing }
IceTipVersionHistoryModel >> commitModels [
	^ commitModels ifNil: [ 
		commitModels := self retrieveHistory collect: [ :each | 
			(IceTipVersionModel 
				repositoryModel: self repositoryModel 
				on: each) 
				beCached ] ]
]

{ #category : #accessing }
IceTipVersionHistoryModel >> descriptionWithDecoration [
	^ self name
]

{ #category : #accessing }
IceTipVersionHistoryModel >> name [
	^ '{1}>>{2}' format: { self entity origin name. self entity selector }
]

{ #category : #private }
IceTipVersionHistoryModel >> retrieveHistory [

	self repositoryModel checkMissing.
	UIManager default 
		informUser: ('Retrieving history of {1}' format: { self entity printString }) 
		during: [ ^ self repositoryModel entity log historyOfMethod: self entity ]
]
