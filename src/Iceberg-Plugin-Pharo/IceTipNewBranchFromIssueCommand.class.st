"
icon
"
Class {
	#name : #IceTipNewBranchFromIssueCommand,
	#superclass : #IceTipCommand,
	#category : #'Iceberg-Plugin-Pharo-Manuscript'
}

{ #category : #testing }
IceTipNewBranchFromIssueCommand class >> browserContextActivation [
	<classAnnotation>
	
	^ CmdContextMenuActivation 
			byItemOf: IcePharoMenuGroup
			order: 20
			for: IceTipRepositoryListContext
]

{ #category : #testing }
IceTipNewBranchFromIssueCommand class >> canBeExecutedInContext: aToolContext [

	^ aToolContext repositoryModel remotes anySatisfy: [ :each | each projectName = 'pharo' ]
]

{ #category : #testing }
IceTipNewBranchFromIssueCommand class >> defaultMenuIconName [
	^ #pharo
]

{ #category : #testing }
IceTipNewBranchFromIssueCommand class >> defaultMenuItemName [
	^ 'Create new branch for issue...'
]

{ #category : #testing }
IceTipNewBranchFromIssueCommand class >> defaultPosition [

	^ #right
]

{ #category : #testing }
IceTipNewBranchFromIssueCommand class >> defaultToolbarItemName [
	^ 'New issue'
]

{ #category : #execution }
IceTipNewBranchFromIssueCommand >> execute [

	(IceTipCheckoutBranchDialog on: self repositoryModel)
		withTypes: (IcePharoManuscriptPlugin new checkoutBranchPanelsOnModel: self repositoryModel);
		openDialogWithSpec
]
