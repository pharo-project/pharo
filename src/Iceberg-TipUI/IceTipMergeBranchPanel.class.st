"
I'm a panel to peform the merge of an existing branch.
"
Class {
	#name : #IceTipMergeBranchPanel,
	#superclass : #IceTipExistingBranchPanel,
	#instVars : [
		'mergeType'
	],
	#category : #'Iceberg-TipUI-View-Branch'
}

{ #category : #accessing }
IceTipMergeBranchPanel >> beSwitchAndMerge [
	self mergeType: IceTipMergeType switchAndMerge
]

{ #category : #actions }
IceTipMergeBranchPanel >> doAccept [
	self validate.
	
	IceTipStandardAction new 
		repository: self model entity; 
		message: ('Verifying merge from {1}' format: { self selectedBranch name });
		onSuccess: [ self selectedBranch previewMerge: self mergeType ];
		execute: [ self validate ]
]

{ #category : #accessing }
IceTipMergeBranchPanel >> mergeType [
	^ mergeType ifNil: [ IceTipMergeType default ]
]

{ #category : #accessing }
IceTipMergeBranchPanel >> mergeType: anObject [
	mergeType := anObject
]
