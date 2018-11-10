"
I will create the missing source subdirectory.

I'm active when: 
	- src subdirectory is not found.
"
Class {
	#name : #IceTipRepairCreateSubdirectory,
	#superclass : #IceTipRepairAction,
	#category : #'Iceberg-TipUI-Commands'
}

{ #category : #accessing }
IceTipRepairCreateSubdirectory class >> help [
	^ 'Create a new subdirectory for the code. Use it only in an empty repository. 
It affects the disk working copy.'
]

{ #category : #accessing }
IceTipRepairCreateSubdirectory class >> icon [
	^ self iconNamed: #smallNew
]

{ #category : #testing }
IceTipRepairCreateSubdirectory class >> isAvailableFor: aRepository [

	^ aRepository isMissing not and: [aRepository isCodeMissing]
]

{ #category : #accessing }
IceTipRepairCreateSubdirectory class >> order [
	^ 17
]

{ #category : #accessing }
IceTipRepairCreateSubdirectory class >> title [
	^ 'Create code subdirectory'
]

{ #category : #executing }
IceTipRepairCreateSubdirectory >> basicExecute [

	self repositoryModel createSourceDirectory.
	"Force the update of the packages"
	self repository workingCopy refreshProjectFromRepository.
	Iceberg announcer announce: (IceRepositoryModified for: self repository)
]
