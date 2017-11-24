"
I answer a morph for each EpEvent.
"
Class {
	#name : #EpMorphVisitor,
	#superclass : #EpEntryContentVisitor,
	#category : #'EpiceaBrowsers-Visitors'
}

{ #category : #private }
EpMorphVisitor >> displayBehavior: aString [ 

	^ { self morphFactory emphasizedStringFor: aString }
]

{ #category : #private }
EpMorphVisitor >> displayBehavior: aClassName method: aSelector [

	^ (self displayBehavior: aClassName), (self displayOperation: '»'), (self displayString: aSelector)
]

{ #category : #private }
EpMorphVisitor >> displayBehaviorComment: aClassName [

	^ (self displayBehavior: aClassName), (self displayOperation: 'comment')
]

{ #category : #private }
EpMorphVisitor >> displayBrowserOperation: aBrowserOperationEvent [

	^ self displayOperation: (
		String streamContents: [ :stream |
			stream nextPutAll: aBrowserOperationEvent title ])
]

{ #category : #private }
EpMorphVisitor >> displayCategory: aCategoryName [

	^ (self displayOperation: 'Package tag'), (self displayString: aCategoryName asString)
]

{ #category : #private }
EpMorphVisitor >> displayClass: aClassName protocol: aProtocolName [

	^ (self displayBehavior: aClassName), (self displayOperation: 'protocol'), (self displayString: aProtocolName)

]

{ #category : #private }
EpMorphVisitor >> displayOperation: aString [ 

	^ self 
		displayString: aString 
		color: self morphFactory softerTextColor
]

{ #category : #private }
EpMorphVisitor >> displayString: aString [

	^ { aString asMorph }
]

{ #category : #private }
EpMorphVisitor >> displayString: aString color: aColor [

	^ { self morphFactory stringFor: aString color: aColor }
]

{ #category : #private }
EpMorphVisitor >> morphFactory [

	^ EpMorphFactory default
]

{ #category : #visitor }
EpMorphVisitor >> visitBehaviorCategoryChange: aClassRecategorizationChange [

	^	(self displayOperation: 'Move'),
		(self displayBehavior: aClassRecategorizationChange behaviorAffectedName), 
		(self displayOperation: 'from tag'),
		(self displayString: aClassRecategorizationChange oldCategory), 
		(self displayOperation: 'to'), 
		(self displayString: aClassRecategorizationChange newCategory)
]

{ #category : #visitor }
EpMorphVisitor >> visitBehaviorCommentChange: aChange [

	^ self displayBehaviorComment: aChange behaviorAffectedName
]

{ #category : #visitor }
EpMorphVisitor >> visitBehaviorCopy: aClassCopy [

	^ (self displayBehavior: aClassCopy definition name), (self displayOperation: 'copyNamed:'), (self displayBehavior: '#', aClassCopy newName)
]

{ #category : #visitor }
EpMorphVisitor >> visitBehaviorNameChange: aClassRenamedChange [

	^	(self displayOperation: 'Rename'), 
		(self displayBehavior: aClassRenamedChange oldName), 
		(self displayOperation: 'as'), 
		(self displayString: aClassRenamedChange newName)
]

{ #category : #visitor }
EpMorphVisitor >> visitCategoryAddition: aCategoryChange [

	^ self displayCategory: aCategoryChange categoryName
]

{ #category : #visitor }
EpMorphVisitor >> visitCategoryRemoval: aCategoryChange [

	^ self displayCategory: aCategoryChange categoryName
]

{ #category : #visitor }
EpMorphVisitor >> visitCategoryRename: aCategoryChange [

	^	(self displayOperation: 'Rename tag'),
		(self displayString: aCategoryChange oldCategoryName),
		(self displayOperation: 'as'),
		(self displayString: aCategoryChange newCategoryName )
]

{ #category : #visitor }
EpMorphVisitor >> visitClassChange: aChange [

	^ self displayBehavior: aChange behaviorAffectedName
]

{ #category : #visitor }
EpMorphVisitor >> visitEvent: anEvent [

	^ self displayString: ''
]

{ #category : #visitor }
EpMorphVisitor >> visitExpressionEvaluation: anExpressionEvaluation [

	^ (self displayOperation: 'DoIt:'), (self displayString:
		(anExpressionEvaluation expression printStringLimitedTo: 50) 
			withSeparatorsCompacted)
]

{ #category : #visitor }
EpMorphVisitor >> visitLogCommentModification: aLogCommentModification [ 

	^ self displayString: aLogCommentModification comment printString
]

{ #category : #visitor }
EpMorphVisitor >> visitMethodAddition: aMethodCreatedChange [

	^ self 
		displayBehavior: aMethodCreatedChange behaviorAffectedName
		method: aMethodCreatedChange methodAdded selector
]

{ #category : #visitor }
EpMorphVisitor >> visitMethodModification: aMethodModified [

	^ self 
		displayBehavior: aMethodModified behaviorAffectedName
		method: aMethodModified oldMethod selector
]

{ #category : #visitor }
EpMorphVisitor >> visitMethodRemoval: aMethodRemoved [

	^ self 
		displayBehavior: aMethodRemoved behaviorAffectedName
		method: aMethodRemoved methodRemoved selector
]

{ #category : #visitor }
EpMorphVisitor >> visitMonticelloVersionSave: aChange [

	^	(self displayOperation: 'MC save'), 
		(self displayString: aChange versionName asString), 
		(self displayOperation: 'on'), 
		(self displayString: aChange repositoryDescription asString)
]

{ #category : #visitor }
EpMorphVisitor >> visitMonticelloVersionsLoad: aMonticelloVersionLoaded [ 

	^	(self displayOperation: 'MC load'), 
		(self displayString: aMonticelloVersionLoaded versionNames asCommaStringAnd)
]

{ #category : #visitor }
EpMorphVisitor >> visitProtocolAddition: aProtocolChange [

	^ self 
		displayClass: aProtocolChange behaviorAffectedName
		protocol: aProtocolChange protocol asString
]

{ #category : #visitor }
EpMorphVisitor >> visitProtocolRemoval: aProtocolChange [

	^ self 
		displayClass: aProtocolChange behaviorAffectedName
		protocol: aProtocolChange protocol asString
]

{ #category : #visitor }
EpMorphVisitor >> visitRedo: aRedoEvent [

	^ self displayBrowserOperation: aRedoEvent
]

{ #category : #visitor }
EpMorphVisitor >> visitRefactoring: aRefactoring [ 

	^  (self displayOperation: 'Refactoring: '), 
		(self displayString: (String streamContents: [ :aStream | 
			aRefactoring summaryOn: aStream ]))
]

{ #category : #visitor }
EpMorphVisitor >> visitSessionEnd: anEvent [

	^ (self displayOperation: 'End:'), 
		(self displayString: anEvent imageName)
]

{ #category : #visitor }
EpMorphVisitor >> visitSessionSnapshot: anEvent [

	^ (self displayOperation: 'Snapshot:'), 
		(self displayString: anEvent imageName)
]

{ #category : #visitor }
EpMorphVisitor >> visitSessionStart: anEvent [

	^ (self displayOperation: 'Start:'), 
		(self displayString: anEvent imageName)
]

{ #category : #visitor }
EpMorphVisitor >> visitTraitChange: aChange [

	^ self displayBehavior: aChange behaviorAffectedName
]

{ #category : #visitor }
EpMorphVisitor >> visitUndo: anUndoEvent [

	^ self displayBrowserOperation: anUndoEvent
]
