"
I implement how to redo EpCodeChange instances.
"
Class {
	#name : 'EpApplyVisitor',
	#superclass : 'EpLogBrowserOperationVisitor',
	#traits : 'EpTCodeChangeVisitor',
	#classTraits : 'EpTCodeChangeVisitor classTrait',
	#category : 'EpiceaBrowsers-Visitors',
	#package : 'EpiceaBrowsers',
	#tag : 'Visitors'
}

{ #category : 'visitor' }
EpApplyVisitor >> visitBehaviorCommentChange: aChange [

	self
		behaviorNamed: aChange behaviorAffectedName
		ifPresent: [ :aClass |
			aClass
				comment: aChange newComment
				stamp: aChange newStamp ]
]

{ #category : 'visitor' }
EpApplyVisitor >> visitBehaviorNameChange: aClassRenameChange [
	(environment classOrTraitNamed: aClassRenameChange oldName)
		rename: aClassRenameChange newName
]

{ #category : 'visitor' }
EpApplyVisitor >> visitBehaviorRepackagedChange: aBehaviorRepackagedChange [

	self behaviorNamed: aBehaviorRepackagedChange behaviorAffectedName ifPresent: [ :aClassOrTrait | aClassOrTrait package: aBehaviorRepackagedChange newPackageName tag: aBehaviorRepackagedChange newTagName ]
]

{ #category : 'visitor' }
EpApplyVisitor >> visitClassAddition: aClassCreation [
	(self compiler evaluate: aClassCreation classAdded definitionSource) fluidInstall.
	(self compiler evaluate: aClassCreation classAdded classSide definitionSource) fluidInstall.
	"update the class comment"
	self
		behaviorNamed: aClassCreation behaviorAffectedName
		ifPresent: [ :aClass |
			aClass
				comment: aClassCreation classAdded comment content
				stamp: aClassCreation classAdded comment stamp ]
]

{ #category : 'visitor' }
EpApplyVisitor >> visitClassModification: aModifiedClassChange [

	(self compiler evaluate: aModifiedClassChange newClass definitionSource) fluidInstall.
	(self compiler evaluate: aModifiedClassChange newClass classSide definitionSource) fluidInstall
]

{ #category : 'visitor' }
EpApplyVisitor >> visitClassRemoval: aClassRemoval [
	environment removeClassNamed: aClassRemoval behaviorAffectedName
]

{ #category : 'visitor' }
EpApplyVisitor >> visitCodeChange: aChange [
	"Default behavior for any change."

	^ self visitEvent: aChange
]

{ #category : 'visitor' }
EpApplyVisitor >> visitMethodAddition: aMethodCreated [

	self
		behaviorNamed: aMethodCreated behaviorAffectedName
		ifPresent: [ :behavior | behavior compile: aMethodCreated sourceCode classified: aMethodCreated protocol ]
]

{ #category : 'visitor' }
EpApplyVisitor >> visitMethodModification: aMethodModified [

	self
		behaviorNamed: aMethodModified behaviorAffectedName
		ifPresent: [ :behavior |
			behavior
				compile: aMethodModified newSourceCode
				classified: aMethodModified newProtocol ]
]

{ #category : 'visitor' }
EpApplyVisitor >> visitMethodRemoval: aMethodRemoval [

	self
		behaviorNamed: aMethodRemoval behaviorAffectedName
		ifPresent: [ :behavior |
			behavior removeSelector: aMethodRemoval selector ]
]

{ #category : 'visitor' }
EpApplyVisitor >> visitPackageAddition: aPackageAddition [

	self packageOrganizer ensurePackage: aPackageAddition packageName
]

{ #category : 'visitor' }
EpApplyVisitor >> visitPackageRemoval: aPackageRemoval [

	self packageOrganizer removePackage: aPackageRemoval packageName
]

{ #category : 'visitor' }
EpApplyVisitor >> visitPackageRename: aPackageRename [

	environment organization renamePackage: aPackageRename oldPackageName to: aPackageRename newPackageName
]

{ #category : 'visitor' }
EpApplyVisitor >> visitPackageTagAddition: aPackageTagAddition [

	environment organization ensureTag: aPackageTagAddition tagName inPackage: aPackageTagAddition packageName
]

{ #category : 'visitor' }
EpApplyVisitor >> visitPackageTagRemoval: aPackageTagRemoval [

	environment organization removeTag: aPackageTagRemoval tagName fromPackage: aPackageTagRemoval packageName
]

{ #category : 'visitor' }
EpApplyVisitor >> visitPackageTagRename: aPackageTagRename [

	environment organization renameTag: aPackageTagRename oldTagName to: aPackageTagRename newTagName inPackage: aPackageTagRename packageName
]

{ #category : 'visitor' }
EpApplyVisitor >> visitProtocolAddition: aProtocolAddition [

	self behaviorNamed: aProtocolAddition behaviorAffectedName ifPresent: [ :behavior | behavior addProtocol: aProtocolAddition protocol ]
]

{ #category : 'visitor' }
EpApplyVisitor >> visitProtocolRemoval: aProtocolRemoved [

	self behaviorNamed: aProtocolRemoved behaviorAffectedName ifPresent: [ :behavior | behavior removeProtocol: aProtocolRemoved protocol ]
]

{ #category : 'visitor' }
EpApplyVisitor >> visitTraitAddition: aChange [

	(self compiler evaluate: aChange traitAdded definitionSource) fluidInstall
]

{ #category : 'visitor' }
EpApplyVisitor >> visitTraitModification: aChange [

	(self compiler evaluate: aChange newTrait definitionSource) fluidInstall
]

{ #category : 'visitor' }
EpApplyVisitor >> visitTraitRemoval: aTraitRemoval [

	environment removeClassNamed: aTraitRemoval behaviorAffectedName
]
