"
I answer a String that represents the new state of an EpEvent.
"
Class {
	#name : 'EpNewStateVisitor',
	#superclass : 'EpContentStringVisitor',
	#category : 'EpiceaBrowsers-Visitors',
	#package : 'EpiceaBrowsers',
	#tag : 'Visitors'
}

{ #category : 'visitor' }
EpNewStateVisitor >> visitBehaviorCommentChange: aChange [

	^ aChange newComment asString
]

{ #category : 'visitor' }
EpNewStateVisitor >> visitBehaviorNameChange: aClassRenameChange [

	^ aClassRenameChange newName
]

{ #category : 'visitor' }
EpNewStateVisitor >> visitBehaviorRepackagedChange: aBehaviorRepackagedChange [

	^ self printTag: aBehaviorRepackagedChange newTagName package: aBehaviorRepackagedChange newPackageName
]

{ #category : 'visitor' }
EpNewStateVisitor >> visitClassAddition: aClassCreatedChange [

	^ self stringForBehavior: aClassCreatedChange classAdded
]

{ #category : 'visitor' }
EpNewStateVisitor >> visitClassModification: aModificationClassChange [

	^ self stringForBehavior: aModificationClassChange newClass
]

{ #category : 'visitor' }
EpNewStateVisitor >> visitExpressionEvaluation: anExpressionEvaluation [

	^ anExpressionEvaluation expression
]

{ #category : 'visitor' }
EpNewStateVisitor >> visitLogCommentModification: anEvent [
	^ anEvent newComment
]

{ #category : 'visitor' }
EpNewStateVisitor >> visitMethodAddition: aMethodCreatedChange [

	^ self printProtocol: aMethodCreatedChange protocol sourceCode: aMethodCreatedChange sourceCode
]

{ #category : 'visitor' }
EpNewStateVisitor >> visitMethodModification: aMethodModification [

	^ self
		printProtocol: aMethodModification newProtocol
		sourceCode: aMethodModification newSourceCode
]

{ #category : 'visitor' }
EpNewStateVisitor >> visitPackageAddition: aPackageAddition [

	^ aPackageAddition packageName
]

{ #category : 'visitor' }
EpNewStateVisitor >> visitPackageRemoval: aPackageRemoval [

	^ aPackageRemoval packageName
]

{ #category : 'visitor' }
EpNewStateVisitor >> visitPackageRename: aPackageRename [

	^ aPackageRename newPackageName
]

{ #category : 'visitor' }
EpNewStateVisitor >> visitPackageTagAddition: aPackageTagAddition [

	^ self printTag: aPackageTagAddition tagName package: aPackageTagAddition packageName
]

{ #category : 'visitor' }
EpNewStateVisitor >> visitPackageTagRename: aPackageTagRename [

	^ self printTag: aPackageTagRename newTagName package: aPackageTagRename packageName
]

{ #category : 'visitor' }
EpNewStateVisitor >> visitProtocolAddition: aProtocolAddition [

	^ aProtocolAddition protocol
]

{ #category : 'visitor' }
EpNewStateVisitor >> visitRedo: aRedoEvent [

	^ self displayBrowserOperation: aRedoEvent
]

{ #category : 'visitor' }
EpNewStateVisitor >> visitTraitAddition: aTraitCreation [

	^ self stringForBehavior: aTraitCreation traitAdded
]

{ #category : 'visitor' }
EpNewStateVisitor >> visitTraitModification: aTraitModificationChange [

	^ self stringForBehavior: aTraitModificationChange newTrait
]
