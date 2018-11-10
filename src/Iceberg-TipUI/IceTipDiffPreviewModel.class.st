Class {
	#name : #IceTipDiffPreviewModel,
	#superclass : #IceTipModel,
	#instVars : [
		'commitish',
		'diffModel'
	],
	#category : #'Iceberg-TipUI-Model'
}

{ #category : #accessing }
IceTipDiffPreviewModel >> calculateDiff [

	^ commitish entity diffTo: self repository workingCopy
]

{ #category : #accessing }
IceTipDiffPreviewModel >> commitish [
	^ commitish entity
]

{ #category : #accessing }
IceTipDiffPreviewModel >> commitish: aCommitish [
	
	commitish := aCommitish
]

{ #category : #accessing }
IceTipDiffPreviewModel >> diff [

	^ diffModel ifNil: [ | diff |
		UIManager default 
			informUser: 'Calculating diff'
			during: [ diff := self calculateDiff ].
			diffModel := IceTipCommitDiffModel
					repositoryModel: self repositoryModel 
					on: diff ]
]

{ #category : #accessing }
IceTipDiffPreviewModel >> repository [
	
	^ commitish repositoryModel entity
]

{ #category : #accessing }
IceTipDiffPreviewModel >> repositoryName [
	
	^ self repository name
]

{ #category : #accessing }
IceTipDiffPreviewModel >> targetName [
	
	^ commitish name
]
