"
I implement how to redo EpCodeChange instances.
"
Class {
	#name : #EpApplyVisitor,
	#superclass : #EpLogBrowserOperationVisitor,
	#traits : 'EpTCodeChangeVisitor',
	#classTraits : 'EpTCodeChangeVisitor classTrait',
	#category : #'EpiceaBrowsers-Visitors'
}

{ #category : #visitor }
EpApplyVisitor >> visitBehaviorCategoryChange: aChange [

	self
		behaviorNamed: aChange behaviorAffectedName
		ifPresent: [:aClassOrTrait | aClassOrTrait category: aChange newCategory ]

]

{ #category : #visitor }
EpApplyVisitor >> visitBehaviorCommentChange: aChange [

	self
		behaviorNamed: aChange behaviorAffectedName
		ifPresent: [ :aClass | 
			aClass 
				classComment: aChange newComment
				stamp: aChange newStamp ]
]

{ #category : #visitor }
EpApplyVisitor >> visitBehaviorNameChange: aClassRenameChange [
	(environment classOrTraitNamed: aClassRenameChange oldName)
		rename: aClassRenameChange newName
]

{ #category : #visitor }
EpApplyVisitor >> visitCategoryAddition: aPackageCreated [
	environment organization addCategory: aPackageCreated categoryName
]

{ #category : #visitor }
EpApplyVisitor >> visitCategoryRemoval: aPackageRemoved [
	environment organization removeCategory: aPackageRemoved categoryName
]

{ #category : #visitor }
EpApplyVisitor >> visitCategoryRename: aChange [

	environment organization
		renameCategory: aChange oldCategoryName
		toBe: aChange newCategoryName
]

{ #category : #visitor }
EpApplyVisitor >> visitClassAddition: aClassCreation [

	self compiler
		evaluate: aClassCreation classAdded definitionSource;
		evaluate: aClassCreation classAdded classSide definitionSource.
]

{ #category : #visitor }
EpApplyVisitor >> visitClassModification: aModifiedClassChange [
	self compiler
		evaluate: aModifiedClassChange newClass definitionSource;
		evaluate: aModifiedClassChange newClass classSide definitionSource
]

{ #category : #visitor }
EpApplyVisitor >> visitClassRemoval: aClassRemoval [
	environment removeClassNamed: aClassRemoval behaviorAffectedName
]

{ #category : #visitor }
EpApplyVisitor >> visitCodeChange: aChange [
	"Default behavior for any change."
		
	^ self visitEvent: aChange
]

{ #category : #visitor }
EpApplyVisitor >> visitMethodAddition: aMethodCreated [

	self 
		behaviorNamed: aMethodCreated behaviorAffectedName
		ifPresent: [ :behavior | 
			behavior
				compile: aMethodCreated sourceCode 
				classified: aMethodCreated protocol ]
]

{ #category : #visitor }
EpApplyVisitor >> visitMethodModification: aMethodModified [

	self 
		behaviorNamed: aMethodModified behaviorAffectedName
		ifPresent: [ :behavior | 
			behavior
				compile: aMethodModified newSourceCode 
				classified: aMethodModified newProtocol ]
]

{ #category : #visitor }
EpApplyVisitor >> visitMethodRemoval: aMethodRemoval [

	self 
		behaviorNamed: aMethodRemoval behaviorAffectedName
		ifPresent: [ :behavior | 
			behavior removeSelector: aMethodRemoval selector ]
]

{ #category : #visitor }
EpApplyVisitor >> visitProtocolAddition: aProtocolAddition [
	self 
		behaviorNamed: aProtocolAddition behaviorAffectedName
		ifPresent: [ :behavior | 
			behavior organization addCategory: aProtocolAddition protocol ]
]

{ #category : #visitor }
EpApplyVisitor >> visitProtocolRemoval: aProtocolRemoved [
	self 
		behaviorNamed: aProtocolRemoved behaviorAffectedName
		ifPresent: [ :behavior | 
			behavior removeProtocol: aProtocolRemoved protocol ]
]

{ #category : #visitor }
EpApplyVisitor >> visitTraitAddition: aChange [

	self compiler evaluate: aChange traitAdded definitionSource
]

{ #category : #visitor }
EpApplyVisitor >> visitTraitModification: aChange [

	self compiler evaluate: aChange newTrait definitionSource

]

{ #category : #visitor }
EpApplyVisitor >> visitTraitRemoval: aTraitRemoval [

	environment removeClassNamed: aTraitRemoval behaviorAffectedName
]
