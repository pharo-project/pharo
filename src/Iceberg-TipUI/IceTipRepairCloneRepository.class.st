"
I'm a repair action to clone again a repository (in case something moved).

I'm active when repository can not be located in disk.

Notice that doing this can lead you to another situation that needs repair (like a detached status)
"
Class {
	#name : #IceTipRepairCloneRepository,
	#superclass : #IceTipRepairAction,
	#category : #'Iceberg-TipUI-Commands'
}

{ #category : #accessing }
IceTipRepairCloneRepository class >> help [
	^ 'Clone again the repository into a new location.

This situation happens when iceberg cannot find the repository in disk.

This action will very probably let you in a detached status.
You will need to re-synchronize your repository with another repair action.'
]

{ #category : #accessing }
IceTipRepairCloneRepository class >> icon [
	^ self iconNamed: #smallAdd
]

{ #category : #testing }
IceTipRepairCloneRepository class >> isAvailableFor: aRepository [
	"active when there is no repository directory"

	^ aRepository isMissing
]

{ #category : #accessing }
IceTipRepairCloneRepository class >> order [
	^ 20
]

{ #category : #accessing }
IceTipRepairCloneRepository class >> title [
	^ 'Clone again this repository'
]

{ #category : #executing }
IceTipRepairCloneRepository >> basicExecute [
	IceTipRegisterRepositoryDialog new 
		beForCloneOfRepository: self repositoryModel entity;
		onAccept: [ 
			Iceberg announcer announce: (IceRepositoryModified for: self repositoryModel entity) ];
		openDialogWithSpec
]
