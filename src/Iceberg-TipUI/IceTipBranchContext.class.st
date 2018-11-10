Class {
	#name : #IceTipBranchContext,
	#superclass : #IceTipContext,
	#instVars : [
		'branchModel'
	],
	#category : #'Iceberg-TipUI-Commands'
}

{ #category : #accessing }
IceTipBranchContext >> branchModel [
	^ branchModel
]

{ #category : #accessing }
IceTipBranchContext >> branchModel: anObject [
	branchModel := anObject
]
