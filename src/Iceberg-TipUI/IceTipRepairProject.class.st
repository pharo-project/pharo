Class {
	#name : #IceTipRepairProject,
	#superclass : #IceTipRepairAction,
	#category : #'Iceberg-TipUI-Commands'
}

{ #category : #accessing }
IceTipRepairProject class >> help [
	^ 'Edit to repository to add project meta-data.
This action will create a .project file in the root of your repository adding meta information such as source code directory, project name, format and so on.

This situation happens because your project does not have project meta-data and Iceberg does not know how to correctly interpret its data.'
]

{ #category : #accessing }
IceTipRepairProject class >> icon [
	^ self iconNamed: #config
]

{ #category : #testing }
IceTipRepairProject class >> isAvailableFor: aRepository [
	"matches if: 
		- workingCopy is atached
		- workingCopy project isUnborn"
	
	^ aRepository isMissing not  
		and: [  aRepository workingCopy isDetached not
			and: [ aRepository head isDetached not
				and: [ aRepository workingCopy project isUnborn ] ] ]
]

{ #category : #accessing }
IceTipRepairProject class >> order [
	^ 30
]

{ #category : #accessing }
IceTipRepairProject class >> title [
	^ 'Create project meta-data'
]

{ #category : #executing }
IceTipRepairProject >> basicExecute [
	(IceTipEditProjectDialog on: (IceBasicProject onRepository: self repositoryModel entity)) 
		openDialogWithSpec
]
