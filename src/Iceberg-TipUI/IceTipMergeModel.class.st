Class {
	#name : #IceTipMergeModel,
	#superclass : #IceTipDiffPreviewModel,
	#instVars : [
		'mergeType',
		'mergeAction'
	],
	#category : #'Iceberg-TipUI-Model'
}

{ #category : #actions }
IceTipMergeModel >> executeMergeActionAndThen: aBlock [ 

	<noCache>
	IceTipStandardAction new 
		repository: self repository;
		message: ('Merging {1} into {2}' format: { self targetName. self repositoryName });
		onSuccessRepositoryModified;
		onSuccess: aBlock;
		execute: [ self merge ]
]

{ #category : #actions }
IceTipMergeModel >> merge [

	mergeType mergeOn: commitish then: [  ].
]

{ #category : #actions }
IceTipMergeModel >> mergeAction [

	^ mergeAction ifNil: [
		mergeAction := IceMerge new
			repository: self repositoryModel entity;
			mergeCommit: commitish entity commit;
			yourself ]
]

{ #category : #accessing }
IceTipMergeModel >> mergeType: aMergeType [

	mergeType := aMergeType
]

{ #category : #accessing }
IceTipMergeModel >> repositoryModel [
	^ commitish repositoryModel
]
