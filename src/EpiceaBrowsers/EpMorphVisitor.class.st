"
I answer a morph for each EpEvent.
"
Class {
	#name : 'EpMorphVisitor',
	#superclass : 'EpEntryContentVisitor',
	#category : 'EpiceaBrowsers-Visitors',
	#package : 'EpiceaBrowsers',
	#tag : 'Visitors'
}

{ #category : 'private' }
EpMorphVisitor >> displayBehavior: aString [

	^ { self morphFactory emphasizedStringFor: aString }
]

{ #category : 'private' }
EpMorphVisitor >> displayBehavior: aClassName method: aSelector [

	^ (self displayBehavior: aClassName), (self displayOperation: '»'), (self displayString: aSelector)
]

{ #category : 'private' }
EpMorphVisitor >> displayBehaviorComment: aClassName [

	^ (self displayBehavior: aClassName), (self displayOperation: 'comment')
]

{ #category : 'private' }
EpMorphVisitor >> displayBrowserOperation: aBrowserOperationEvent [

	^ self displayOperation: (
		String streamContents: [ :stream |
			stream nextPutAll: aBrowserOperationEvent title ])
]

{ #category : 'private' }
EpMorphVisitor >> displayClass: aClassName protocol: aProtocolName [

	^ (self displayBehavior: aClassName), (self displayOperation: 'protocol'), (self displayString: aProtocolName)
]

{ #category : 'private' }
EpMorphVisitor >> displayOperation: aString [

	^ self
		displayString: aString
		color: self morphFactory softerTextColor
]

{ #category : 'private' }
EpMorphVisitor >> displayPackage: aPackageName [

	^ (self displayOperation: 'Package') , (self displayString: aPackageName asString)
]

{ #category : 'private' }
EpMorphVisitor >> displayString: aString [

	^ { aString asMorph }
]

{ #category : 'private' }
EpMorphVisitor >> displayString: aString color: aColor [

	^ { self morphFactory stringFor: aString color: aColor }
]

{ #category : 'private' }
EpMorphVisitor >> displayTag: aTagName inPackage: aPackageName [

	^ (self displayOperation: 'Tag') , (self displayString: aTagName) , (self displayOperation: ' in package') , (self displayString: aPackageName)
]

{ #category : 'private' }
EpMorphVisitor >> morphFactory [

	^ EpMorphFactory default
]

{ #category : 'visitor' }
EpMorphVisitor >> visitBehaviorCommentChange: aChange [

	^ self displayBehaviorComment: aChange behaviorAffectedName
]

{ #category : 'visitor' }
EpMorphVisitor >> visitBehaviorCopy: aClassCopy [

	^ (self displayBehavior: aClassCopy definition name), (self displayOperation: 'copyNamed:'), (self displayBehavior: '#', aClassCopy newName)
]

{ #category : 'visitor' }
EpMorphVisitor >> visitBehaviorNameChange: aClassRenamedChange [

	^	(self displayOperation: 'Rename'),
		(self displayBehavior: aClassRenamedChange oldName),
		(self displayOperation: 'as'),
		(self displayString: aClassRenamedChange newName)
]

{ #category : 'visitor' }
EpMorphVisitor >> visitBehaviorRepackagedChange: aBehaviorRepackagedChange [

	^ (self displayOperation: 'Move ') , (self displayBehavior: aBehaviorRepackagedChange behaviorAffectedName) , (self displayOperation: 'from ')
	  , (self displayTag: aBehaviorRepackagedChange oldTagName inPackage: aBehaviorRepackagedChange oldPackageName) , (self displayOperation: ' to ')
	  , (self displayTag: aBehaviorRepackagedChange newTagName inPackage: aBehaviorRepackagedChange newPackageName)
]

{ #category : 'visitor' }
EpMorphVisitor >> visitClassChange: aChange [

	^ self displayBehavior: aChange behaviorAffectedName
]

{ #category : 'visitor' }
EpMorphVisitor >> visitEvent: anEvent [

	^ self displayString: ''
]

{ #category : 'visitor' }
EpMorphVisitor >> visitExpressionEvaluation: anExpressionEvaluation [

	^ (self displayOperation: 'DoIt:'), (self displayString:
		(anExpressionEvaluation expression printStringLimitedTo: 50)
			withSeparatorsCompacted)
]

{ #category : 'visitor' }
EpMorphVisitor >> visitLogCommentModification: aLogCommentModification [

	^ self displayString: aLogCommentModification comment printString
]

{ #category : 'visitor' }
EpMorphVisitor >> visitMethodAddition: aMethodCreatedChange [

	^ self
		displayBehavior: aMethodCreatedChange behaviorAffectedName
		method: aMethodCreatedChange methodAdded selector
]

{ #category : 'visitor' }
EpMorphVisitor >> visitMethodModification: aMethodModified [

	^ self
		displayBehavior: aMethodModified behaviorAffectedName
		method: aMethodModified oldMethod selector
]

{ #category : 'visitor' }
EpMorphVisitor >> visitMethodRemoval: aMethodRemoved [

	^ self
		displayBehavior: aMethodRemoved behaviorAffectedName
		method: aMethodRemoved methodRemoved selector
]

{ #category : 'visitor' }
EpMorphVisitor >> visitMonticelloVersionSave: aChange [

	^	(self displayOperation: 'MC save'),
		(self displayString: aChange versionName asString),
		(self displayOperation: 'on'),
		(self displayString: aChange repositoryDescription asString)
]

{ #category : 'visitor' }
EpMorphVisitor >> visitMonticelloVersionsLoad: aMonticelloVersionLoaded [

	^	(self displayOperation: 'MC load'),
		(self displayString: aMonticelloVersionLoaded versionNames asCommaStringAnd)
]

{ #category : 'visitor' }
EpMorphVisitor >> visitPackageAddition: aPackageAddition [

	^ self displayPackage: aPackageAddition packageName
]

{ #category : 'visitor' }
EpMorphVisitor >> visitPackageRemoval: aPackageRemoval [

	^ self displayPackage: aPackageRemoval packageName
]

{ #category : 'visitor' }
EpMorphVisitor >> visitPackageRename: aPackageRename [

	^ (self displayOperation: 'Rename package') , (self displayString: aPackageRename oldPackageName) , (self displayOperation: 'as')
	  , (self displayString: aPackageRename newPackageName)
]

{ #category : 'visitor' }
EpMorphVisitor >> visitPackageTagAddition: aPackageTagAddition [

	^ self displayTag: aPackageTagAddition tagName inPackage: aPackageTagAddition packageName
]

{ #category : 'visitor' }
EpMorphVisitor >> visitPackageTagRemoval: aPackageTagRemoval [

	^ self displayTag: aPackageTagRemoval tagName inPackage: aPackageTagRemoval packageName
]

{ #category : 'visitor' }
EpMorphVisitor >> visitPackageTagRename: aPackageTagRename [

	^ (self displayOperation: 'Rename tag') , (self displayString: aPackageTagRename oldTagName) , (self displayOperation: 'as')
	  , (self displayString: aPackageTagRename newTagName) , (self displayOperation: 'in package') , (self displayString: aPackageTagRename packageName)
]

{ #category : 'visitor' }
EpMorphVisitor >> visitProtocolAddition: aProtocolChange [

	^ self displayClass: aProtocolChange behaviorAffectedName protocol: aProtocolChange protocol
]

{ #category : 'visitor' }
EpMorphVisitor >> visitProtocolRemoval: aProtocolChange [

	^ self displayClass: aProtocolChange behaviorAffectedName protocol: aProtocolChange protocol
]

{ #category : 'visitor' }
EpMorphVisitor >> visitRedo: aRedoEvent [

	^ self displayBrowserOperation: aRedoEvent
]

{ #category : 'visitor' }
EpMorphVisitor >> visitRefactoring: aRefactoring [

	^  (self displayOperation: 'Refactoring: '),
		(self displayString: (String streamContents: [ :aStream |
			aRefactoring summaryOn: aStream ]))
]

{ #category : 'visitor' }
EpMorphVisitor >> visitSessionEnd: anEvent [

	^ (self displayOperation: 'End:'),
		(self displayString: anEvent imageName)
]

{ #category : 'visitor' }
EpMorphVisitor >> visitSessionSnapshot: anEvent [

	^ (self displayOperation: 'Snapshot:'),
		(self displayString: anEvent imageName)
]

{ #category : 'visitor' }
EpMorphVisitor >> visitSessionStart: anEvent [

	^ (self displayOperation: 'Start:'),
		(self displayString: anEvent imageName)
]

{ #category : 'visitor' }
EpMorphVisitor >> visitTraitChange: aChange [

	^ self displayBehavior: aChange behaviorAffectedName
]

{ #category : 'visitor' }
EpMorphVisitor >> visitUndo: anUndoEvent [

	^ self displayBrowserOperation: anUndoEvent
]
