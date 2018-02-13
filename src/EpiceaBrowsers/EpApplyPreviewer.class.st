"
I take as input a collection of  EpCodeChange instances and output the equivalent of such changes as if they were applied in current image.
"
Class {
	#name : #EpApplyPreviewer,
	#superclass : #Object,
	#traits : 'EpTCodeChangeVisitor',
	#classTraits : 'EpTCodeChangeVisitor classTrait',
	#instVars : [
		'environment',
		'logBrowserModel',
		'sourceEntries'
	],
	#category : #'EpiceaBrowsers-Visitors'
}

{ #category : #private }
EpApplyPreviewer >> behaviorNamed: aSymbol ifPresent: presentBlock [

	^ (self environment classOrTraitNamed: aSymbol) 
		ifNil: [ self ]
		ifNotNil: presentBlock
]

{ #category : #accessing }
EpApplyPreviewer >> environment [

	^ environment ifNil: [ self class environment ]
]

{ #category : #accessing }
EpApplyPreviewer >> environment: anObject [
	environment := anObject
]

{ #category : #private }
EpApplyPreviewer >> includesCategory: aString [

	^ self environment organization includesCategory: aString
]

{ #category : #visitor }
EpApplyPreviewer >> visitBehaviorCategoryChange: aChange [

	self
		behaviorNamed: aChange behaviorAffectedName
		ifPresent: [ :behavior |
			^ behavior category = aChange behaviorAffected category
				ifTrue: [ #() ]
				ifFalse: [ {
					EpBehaviorCategoryChange
						oldCategory: aChange oldCategory
						newCategory: aChange newCategory
						class: behavior 
					}
				]
			].
			
	^ #()
]

{ #category : #visitor }
EpApplyPreviewer >> visitBehaviorCommentChange: aChange [

	self
		behaviorNamed: aChange behaviorAffectedName
		ifPresent: [ :aBehavior | 
			(aBehavior organization classComment = aChange newComment) ifFalse: [ 
				^ { 
					EpBehaviorCommentChange 
						newWithBehavior: aBehavior
						oldComment: aBehavior organization classComment
						newComment: aChange newComment 
						oldStamp: aBehavior organization commentStamp
						newStamp: aChange newStamp 
				} ] ].

	^ #()
]

{ #category : #visitor }
EpApplyPreviewer >> visitBehaviorNameChange: aChange [

	self 
		behaviorNamed: aChange oldName
		ifPresent: [ :behavior | 
			^ { EpBehaviorNameChange 
					oldName: aChange oldName
					newName: aChange newName
					class: behavior } ].

	^ #()
]

{ #category : #visitor }
EpApplyPreviewer >> visitCategoryAddition: aChange [

	^ (self includesCategory: aChange categoryName)
			ifTrue: [ #() ]
			ifFalse: [ { aChange } ]
]

{ #category : #visitor }
EpApplyPreviewer >> visitCategoryRemoval: aChange [

	^ (self includesCategory: aChange categoryName)
			ifTrue: [ { aChange } ]
			ifFalse: [ #() ]
]

{ #category : #visitor }
EpApplyPreviewer >> visitCategoryRename: aChange [

	^ (self includesCategory: aChange oldCategoryName)
		ifTrue: [ { aChange } ]
		ifFalse: [ #() ]
]

{ #category : #visitor }
EpApplyPreviewer >> visitClassChange: aChange [

	self
		behaviorNamed: aChange behaviorAffectedName
		ifPresent: [ :aClass |
			^ aClass definition = aChange behaviorAffected definition
				ifTrue: [ #() ]
				ifFalse: [ { EpClassModification oldClass: aClass newClass: aChange behaviorAffected } ]
			].
	
	^ { EpClassAddition class: aChange behaviorAffected }
]

{ #category : #visitor }
EpApplyPreviewer >> visitClassRemoval: aChange [

	self 
		behaviorNamed: aChange behaviorAffectedName
		ifPresent: [ :behavior | ^ { EpClassRemoval class: behavior } ].

	^ #()
]

{ #category : #visitor }
EpApplyPreviewer >> visitCodeChange: aChange [
	"Default behavior for any change."
		
	self error: 'Shouldn''t happen'. ^ {aChange}
]

{ #category : #visitor }
EpApplyPreviewer >> visitEvent: aChange [
	"Generic events do not apply any code change"

	^ #()
]

{ #category : #visitor }
EpApplyPreviewer >> visitMethodChange: aChange [
	"Addition and Modification come here"

	self 
		behaviorNamed: aChange behaviorAffectedName
		ifPresent: [ :behavior | 
			^behavior 
				compiledMethodAt: aChange methodAffectedSelector
				ifPresent: [ :method |
					(method protocol ~= aChange methodAffectedProtocol or: [
					method sourceCode ~= aChange methodAffectedSourceCode ]) 
						ifTrue: [ { EpMethodModification oldMethod: method newMethod: aChange methodAffected } ]
						ifFalse: [ #() ] ]
				ifAbsent: [ 
					{ EpMethodAddition method: aChange methodAffected } ] ].
	
	^ { aChange }

]

{ #category : #visitor }
EpApplyPreviewer >> visitMethodRemoval: aChange [

	self 
		behaviorNamed: aChange behaviorAffectedName
		ifPresent: [ :behavior |
			"If method comes from trait, removing has no impact."
			^ behavior 
				compiledMethodAt: aChange methodAffectedSelector
				ifPresent: [ :method | method isFromTrait not
					ifTrue: [ { EpMethodRemoval method: method } ]
					ifFalse: [ #() ] ]
				ifAbsent: [ #() ] ].

	^ #() "No impact if container does not exist"

]

{ #category : #visitor }
EpApplyPreviewer >> visitProtocolAddition: aChange [

	self 
		behaviorNamed: aChange behaviorAffectedName
		ifPresent: [ :behavior |
			^(behavior organization protocolOrganizer hasProtocolNamed: aChange protocol)
				ifTrue: [ #() ]
				ifFalse: [ { EpProtocolAddition behavior: behavior protocol: aChange protocol } ] ].

	^ #()
]

{ #category : #visitor }
EpApplyPreviewer >> visitProtocolRemoval: aChange [

	self 
		behaviorNamed: aChange behaviorAffectedName
		ifPresent: [ :behavior |
			^(behavior organization protocolOrganizer hasProtocolNamed: aChange protocol)
				ifTrue: [ { EpProtocolRemoval behavior: behavior protocol: aChange protocol } ]
				ifFalse: [ #() ] ].

	^ #()
]

{ #category : #visitor }
EpApplyPreviewer >> visitTraitChange: aChange [

	self
		behaviorNamed: aChange behaviorAffectedName
		ifPresent: [ :aTrait |
			^ { EpTraitModification oldTrait: aTrait newTrait: aChange behaviorAffected } ].
	
	^ { EpTraitAddition trait: aChange behaviorAffected }
]

{ #category : #visitor }
EpApplyPreviewer >> visitTraitRemoval: aChange [

	self 
		behaviorNamed: aChange behaviorAffectedName
		ifPresent: [ :behavior | ^ { EpTraitRemoval trait: behavior } ].

	^ #()
]
