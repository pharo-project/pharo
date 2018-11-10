"
I will discard image changes and I will restore source from current repository HEAD.

I'm active when: 
	- working copy is detached 
	- repository HEAD is NOT detached
"
Class {
	#name : #IceTipRepairDiscardAndLoad,
	#superclass : #IceTipRepairAction,
	#category : #'Iceberg-TipUI-Commands'
}

{ #category : #accessing }
IceTipRepairDiscardAndLoad class >> help [
	^ 'Discard all changes in your image and checkout the code in the repository.
This action will align your image with your repository.

This situation happens because the code loaded in your image does not correspond with the status of your repository.

Opens a preview window before doing any change.'
]

{ #category : #accessing }
IceTipRepairDiscardAndLoad class >> icon [
	^ self iconNamed: #refresh
]

{ #category : #testing }
IceTipRepairDiscardAndLoad class >> isAvailableFor: aRepository [
	"matches if: 
		- workingCopy is detached
		- HEAD is NOT detached"

	^ aRepository isMissing not  
		and: [ aRepository workingCopy isDetached 
		and: [ aRepository head isDetached not ] ]
]

{ #category : #accessing }
IceTipRepairDiscardAndLoad class >> order [
	^ 40
]

{ #category : #accessing }
IceTipRepairDiscardAndLoad class >> title [
	^ 'Discard image changes and load repository version'
]

{ #category : #executing }
IceTipRepairDiscardAndLoad >> basicExecute [
			
	(IceTipCheckoutPreviewBrowser onBranch: self repositoryModel branchModel) openWithSpec
]

{ #category : #private }
IceTipRepairDiscardAndLoad >> finishSuccess [
	super finishSuccess.
	Iceberg announcer announce: (IceRepositoryModified for: self repository)
]
