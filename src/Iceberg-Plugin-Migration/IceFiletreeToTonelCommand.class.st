"
I'm a command to enable the filetree to tonel conversion.
I will be active in repository list browser, when repository is in filetree format.

"
Class {
	#name : #IceFiletreeToTonelCommand,
	#superclass : #IceTipCommand,
	#category : #'Iceberg-Plugin-Migration'
}

{ #category : #activation }
IceFiletreeToTonelCommand class >> browserRepositoryListActivation [
	<classAnnotation>
	
	^ CmdContextMenuActivation
		byItemOf: CmdExtraMenuGroup
		order: 100
		for: IceTipRepositoryListContext
]

{ #category : #testing }
IceFiletreeToTonelCommand class >> canBeExecutedInContext: aToolContext [

	| repository |
	repository := aToolContext repositoryModel entity.
	^ repository isMissing not 
		and: [ repository workingCopy isDetached not
			and: [ repository workingCopy writerClass id = #filetree
				and: [ repository head isUnbornBranch not ] ] ]
]

{ #category : #accessing }
IceFiletreeToTonelCommand >> defaultMenuItemName [
	^ 'Convert repository sources to tonel'
]

{ #category : #execution }
IceFiletreeToTonelCommand >> execute [

	(IceTipFiletreeToTonelDialog on: self repositoryModel entity) openDialogWithSpec
]
