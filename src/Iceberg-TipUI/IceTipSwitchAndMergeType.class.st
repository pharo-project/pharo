"
I'm a switch and merge merging type. 
I will switch first to the new branch, and then I will perform a merge with current reference commit.

	1. Switch to merging branch.
	2. Merging branch <- reference commit
	
This strategy is used to re-attach image when it is detached from current development branch.
"
Class {
	#name : #IceTipSwitchAndMergeType,
	#superclass : #IceTipMergeType,
	#category : #'Iceberg-TipUI-View-Branch'
}

{ #category : #executing }
IceTipSwitchAndMergeType >> mergeOn: aModel then: aBlock [
	^ aModel switchAndMergeThen: aBlock
]
