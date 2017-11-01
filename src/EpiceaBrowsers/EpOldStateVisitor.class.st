"
I answer a String that represents the old state of an EpEvent.
"
Class {
	#name : #EpOldStateVisitor,
	#superclass : #EpContentStringVisitor,
	#category : 'EpiceaBrowsers-Visitors'
}

{ #category : #visitor }
EpOldStateVisitor >> visitBehaviorCategoryChange: aRecategorizationChange [

	^ aRecategorizationChange oldCategory
	    
]

{ #category : #visitor }
EpOldStateVisitor >> visitBehaviorCommentChange: aChange [

	^ aChange oldComment asString
]

{ #category : #visitor }
EpOldStateVisitor >> visitBehaviorNameChange: aClassRenameChange [

	^ aClassRenameChange oldName
	    
]

{ #category : #visitor }
EpOldStateVisitor >> visitCategoryRemoval: aClassCategoryRemovedChange [

	^ aClassCategoryRemovedChange categoryName
]

{ #category : #visitor }
EpOldStateVisitor >> visitCategoryRename: aClassCategoryRenamedChange [

	^ aClassCategoryRenamedChange oldCategoryName
]

{ #category : #visitor }
EpOldStateVisitor >> visitClassModification: aModificationClassChange [

	^ self stringForBehavior: aModificationClassChange oldClass
]

{ #category : #visitor }
EpOldStateVisitor >> visitClassRemoval: aClassRemovalChange [

	^ self stringForBehavior: aClassRemovalChange classRemoved
	    
]

{ #category : #visitor }
EpOldStateVisitor >> visitLogCommentModification: anEvent [

	^ anEvent oldComment
]

{ #category : #visitor }
EpOldStateVisitor >> visitMethodModification: aMethodModification [

	^ self 
		printProtocol: aMethodModification oldProtocol  
		sourceCode: aMethodModification oldSourceCode
]

{ #category : #visitor }
EpOldStateVisitor >> visitMethodRemoval: aMethodRemoval [

	^ self 
		printProtocol: aMethodRemoval protocol
		sourceCode: aMethodRemoval sourceCode
]

{ #category : #visitor }
EpOldStateVisitor >> visitProtocolRemoval: aProtocolRemoval [

	^ aProtocolRemoval protocol
]

{ #category : #visitor }
EpOldStateVisitor >> visitTraitModification: aTraitModificationChange [

	^ self stringForBehavior: aTraitModificationChange oldTrait
	    
]

{ #category : #visitor }
EpOldStateVisitor >> visitTraitRemoval:  aTraitRemoval [

	^ self stringForBehavior: aTraitRemoval traitRemoved
]

{ #category : #visitor }
EpOldStateVisitor >> visitUndo: anUndoEvent [

	^ self displayBrowserOperation: anUndoEvent
]
