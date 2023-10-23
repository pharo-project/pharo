"
I convert a code change to it's inverse operation. For example, a class addition into a class removal.
"
Class {
	#name : 'EpInverseVisitor',
	#superclass : 'EpLogBrowserOperationVisitor',
	#traits : 'EpTCodeChangeVisitor',
	#classTraits : 'EpTCodeChangeVisitor classTrait',
	#category : 'EpiceaBrowsers-Visitors',
	#package : 'EpiceaBrowsers',
	#tag : 'Visitors'
}

{ #category : 'visitor' }
EpInverseVisitor >> visitBehaviorCommentChange: aChange [

	^ EpBehaviorCommentChange
			newWithBehavior: aChange behaviorAffected
			oldComment: aChange newComment
			newComment: aChange oldComment
			oldStamp: aChange newStamp
			newStamp: aChange oldStamp
]

{ #category : 'visitor' }
EpInverseVisitor >> visitBehaviorNameChange: aChange [

	^ EpBehaviorNameChange
			oldName: aChange newName
			newName: aChange oldName
			class: aChange behaviorAffected
]

{ #category : 'visitor' }
EpInverseVisitor >> visitBehaviorRepackagedChange: aBehaviorRepackagedChange [

	^ EpBehaviorRepackagedChange
		  oldPackage: aBehaviorRepackagedChange newPackageName
		  oldTag: aBehaviorRepackagedChange newTagName
		  newPackage: aBehaviorRepackagedChange oldPackageName
		  newTag: aBehaviorRepackagedChange oldTagName
		  class: aBehaviorRepackagedChange behaviorAffected
]

{ #category : 'visitor' }
EpInverseVisitor >> visitClassAddition: aChange [

	^ EpClassRemoval class: aChange behaviorAffected
]

{ #category : 'visitor' }
EpInverseVisitor >> visitClassModification: aChange [

	^ EpClassModification oldClass: aChange newClass newClass: aChange oldClass
]

{ #category : 'visitor' }
EpInverseVisitor >> visitClassRemoval: aChange [

	^ EpClassAddition class: aChange behaviorAffected
]

{ #category : 'visitor' }
EpInverseVisitor >> visitCodeChange: aChange [
	^ self error: 'Should not be reached'
]

{ #category : 'visitor' }
EpInverseVisitor >> visitMethodAddition: aChange [

	^ EpMethodRemoval method: aChange methodAffected
]

{ #category : 'visitor' }
EpInverseVisitor >> visitMethodModification: aChange [

	^ EpMethodModification oldMethod: aChange newMethod newMethod: aChange oldMethod
]

{ #category : 'visitor' }
EpInverseVisitor >> visitMethodRemoval: aChange [

	^ EpMethodAddition method: aChange methodAffected
]

{ #category : 'visitor' }
EpInverseVisitor >> visitPackageAddition: aPackageAddition [

	^ EpPackageRemoval packageName: aPackageAddition packageName
]

{ #category : 'visitor' }
EpInverseVisitor >> visitPackageRemoval: aPackageRemoval [

	^ EpPackageAddition packageName: aPackageRemoval packageName
]

{ #category : 'visitor' }
EpInverseVisitor >> visitPackageRename: aPackageRename [

	^ EpPackageRename oldName: aPackageRename newPackageName newName: aPackageRename oldPackageName
]

{ #category : 'visitor' }
EpInverseVisitor >> visitPackageTagAddition: aPackageTagAddition [

	^ EpPackageTagRemoval tagName: aPackageTagAddition tagName packageName: aPackageTagAddition packageName
]

{ #category : 'visitor' }
EpInverseVisitor >> visitPackageTagRemoval: aPackageTagRemoval [

	^ EpPackageTagAddition tagName: aPackageTagRemoval tagName packageName: aPackageTagRemoval packageName
]

{ #category : 'visitor' }
EpInverseVisitor >> visitPackageTagRename: aPackageTagRename [

	^ EpPackageTagRename oldName: aPackageTagRename newTagName newName: aPackageTagRename oldTagName packageName: aPackageTagRename packageName
]

{ #category : 'visitor' }
EpInverseVisitor >> visitProtocolAddition: aChange [

	^ EpProtocolRemoval behavior: aChange behaviorAffected protocol: aChange protocol
]

{ #category : 'visitor' }
EpInverseVisitor >> visitProtocolRemoval: aChange [

	^ EpProtocolAddition behavior: aChange behaviorAffected protocol: aChange protocol
]

{ #category : 'visitor' }
EpInverseVisitor >> visitTraitAddition: aChange [

	^ EpTraitRemoval trait: aChange behaviorAffected
]

{ #category : 'visitor' }
EpInverseVisitor >> visitTraitModification: aTraitModification [

  	^ EpTraitModification
    		  oldTrait: aTraitModification newTrait
          		  newTrait: aTraitModification oldTrait
]

{ #category : 'visitor' }
EpInverseVisitor >> visitTraitRemoval: aChange [

	^ EpTraitAddition trait: aChange behaviorAffected
]
