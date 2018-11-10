Class {
	#name : #IceTipCalculateDirtyPackagesCommand,
	#superclass : #IceTipRepositoryCommand,
	#instVars : [
		'item'
	],
	#category : #'Iceberg-TipUI-Commands'
}

{ #category : #activation }
IceTipCalculateDirtyPackagesCommand class >> browserRepositoryListActivation [
	<classAnnotation>
	
	^ CmdContextMenuActivation
		byItemOf: CmdExtraMenuGroup
		for: IceTipRepositoryListContext
]

{ #category : #accessing }
IceTipCalculateDirtyPackagesCommand class >> defaultMenuIconName [
	^ #repair
]

{ #category : #accessing }
IceTipCalculateDirtyPackagesCommand class >> defaultMenuItemName [
	^ 'Recalculate dirty packages'
]

{ #category : #execution }
IceTipCalculateDirtyPackagesCommand >> execute [
	item ifNil: [ ^ self ].
	IceTipStandardAction new
		repository: item entity;
		message: 'Recalculating Dirty Packages';
		onSuccessRepositoryModified;
		execute: [ item entity workingCopy forceCalculateDirtyPackages ]
]

{ #category : #execution }
IceTipCalculateDirtyPackagesCommand >> readParametersFromContext: aToolContext [
	super readParametersFromContext: aToolContext.
	item := aToolContext item
]
