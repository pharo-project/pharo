Class {
	#name : #IceTipBranchFilter,
	#superclass : #IceTipTreeFilter,
	#category : #'Iceberg-TipUI-View'
}

{ #category : #testing }
IceTipBranchFilter >> matches: treeModel [
	^ treeModel model name asLowercase includesSubstring: self pattern
]
