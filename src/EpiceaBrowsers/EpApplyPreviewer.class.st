"
I take as input a collection of  EpCodeChange instances and output the equivalent of such changes as if they were applied in current image.
"
Class {
	#name : 'EpApplyPreviewer',
	#superclass : 'Object',
	#traits : 'EpTCodeChangeVisitor',
	#classTraits : 'EpTCodeChangeVisitor classTrait',
	#instVars : [
		'environment'
	],
	#category : 'EpiceaBrowsers-Visitors',
	#package : 'EpiceaBrowsers',
	#tag : 'Visitors'
}

{ #category : 'private' }
EpApplyPreviewer >> behaviorNamed: aSymbol ifPresent: presentBlock [

	^ (self environment classOrTraitNamed: aSymbol)
		ifNil: [ self ]
		ifNotNil: presentBlock
]

{ #category : 'accessing' }
EpApplyPreviewer >> environment [

	^ environment ifNil: [ self class environment ]
]

{ #category : 'accessing' }
EpApplyPreviewer >> environment: anObject [
	environment := anObject
]

{ #category : 'visitor' }
EpApplyPreviewer >> visitBehaviorCommentChange: aChange [

	self
		behaviorNamed: aChange behaviorAffectedName
		ifPresent: [ :aBehavior |
			(aBehavior comment = aChange newComment) ifFalse: [
				^ {
					EpBehaviorCommentChange
						newWithBehavior: aBehavior
						oldComment: aBehavior comment
						newComment: aChange newComment
						oldStamp: aBehavior commentStamp
						newStamp: aChange newStamp
				} ] ].

	^ #()
]

{ #category : 'visitor' }
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

{ #category : 'visitor' }
EpApplyPreviewer >> visitBehaviorRepackagedChange: aBehaviorRepackagedChange [

	| classRepackaged |
	classRepackaged := aBehaviorRepackagedChange classAffected.
	self behaviorNamed: classRepackaged name ifPresent: [ :behavior |
		^ (behavior package name = classRepackaged package name and: [ behavior packageTag name = classRepackaged packageTag ])
			  ifTrue: [ #(  ) ]
			  ifFalse: [
				  { (EpBehaviorRepackagedChange
					   oldPackage: aBehaviorRepackagedChange oldPackageName
					   oldTag: aBehaviorRepackagedChange oldTagName
					   newPackage: aBehaviorRepackagedChange newPackageName
					   newTag: aBehaviorRepackagedChange newTagName
					   class: behavior) } ] ].

	^ #(  )
]

{ #category : 'visitor' }
EpApplyPreviewer >> visitClassChange: aChange [

	self
		behaviorNamed: aChange behaviorAffectedName
		ifPresent: [ :aClass |
			^ aClass definitionString = aChange behaviorAffected definition
				  ifTrue: [ #(  ) ]
				  ifFalse: [
					  { (EpClassModification
						   oldClass: aClass
						   newClass: aChange behaviorAffected) } ] ].

	^ { (EpClassAddition class: aChange behaviorAffected) }
]

{ #category : 'visitor' }
EpApplyPreviewer >> visitClassRemoval: aChange [

	self
		behaviorNamed: aChange behaviorAffectedName
		ifPresent: [ :behavior | ^ { EpClassRemoval class: behavior } ].

	^ #()
]

{ #category : 'visitor' }
EpApplyPreviewer >> visitCodeChange: aChange [
	"Default behavior for any change."

	self error: 'Shouldn''t happen'. ^ {aChange}
]

{ #category : 'visitor' }
EpApplyPreviewer >> visitEvent: aChange [
	"Generic events do not apply any code change"

	^ #()
]

{ #category : 'visitor' }
EpApplyPreviewer >> visitMethodChange: aChange [
	"Addition and Modification come here"

	self
		behaviorNamed: aChange behaviorAffectedName
		ifPresent: [ :behavior |
			^ behavior
				compiledMethodAt: aChange methodAffectedSelector
				ifPresent: [ :method |
					(method protocol name ~= aChange methodAffectedProtocol
						or: [ method sourceCode ~= aChange methodAffectedSourceCode ])
						ifTrue: [ {(EpMethodModification
								oldMethod: method
								newMethod: aChange methodAffected)} ]
						ifFalse: [ #() ] ]
				ifAbsent: [ {(EpMethodAddition method: aChange methodAffected)} ] ].
	^ {aChange}
]

{ #category : 'visitor' }
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

{ #category : 'visitor' }
EpApplyPreviewer >> visitPackageAddition: aPackageAddition [

	^ (self packageOrganizer hasPackage: aPackageAddition packageName)
		  ifTrue: [ {  } ]
		  ifFalse: [ { aPackageAddition } ]
]

{ #category : 'visitor' }
EpApplyPreviewer >> visitPackageRemoval: aPackageRemoval [

	^ (self packageOrganizer hasPackage: aPackageRemoval packageName)
		  ifTrue: [ { aPackageRemoval } ]
		  ifFalse: [ {  } ]
]

{ #category : 'visitor' }
EpApplyPreviewer >> visitPackageRename: aPackageRename [

	^ (self packageOrganizer hasPackage: aPackageRename oldPackageName)
		  ifTrue: [ { aPackageRename } ]
		  ifFalse: [ #(  ) ]
]

{ #category : 'visitor' }
EpApplyPreviewer >> visitPackageTagAddition: aPackageTagAddition [

	^ (self packageOrganizer hasTag: aPackageTagAddition tagName inPackage: aPackageTagAddition packageName)
		  ifTrue: [ #(  ) ]
		  ifFalse: [ { aPackageTagAddition } ]
]

{ #category : 'visitor' }
EpApplyPreviewer >> visitPackageTagRemoval: aPackageTagRemoval [

	^ (self packageOrganizer hasTag: aPackageTagRemoval tagName inPackage: aPackageTagRemoval packageName)
		  ifTrue: [ { aPackageTagRemoval } ]
		  ifFalse: [ #(  ) ]
]

{ #category : 'visitor' }
EpApplyPreviewer >> visitPackageTagRename: aPackageTagRename [

	^ (self packageOrganizer hasTag: aPackageTagRename oldTagName inPackage: aPackageTagRename packageName)
		  ifTrue: [ { aPackageTagRename } ]
		  ifFalse: [ #(  ) ]
]

{ #category : 'visitor' }
EpApplyPreviewer >> visitProtocolAddition: aChange [

	self behaviorNamed: aChange behaviorAffectedName ifPresent: [ :behavior |
		^ (behavior hasProtocol: aChange protocol)
			  ifTrue: [ #(  ) ]
			  ifFalse: [ { (EpProtocolAddition behavior: behavior protocol: aChange protocol) } ] ].

	^ #(  )
]

{ #category : 'visitor' }
EpApplyPreviewer >> visitProtocolRemoval: aChange [

	self behaviorNamed: aChange behaviorAffectedName ifPresent: [ :behavior |
		^ (behavior hasProtocol: aChange protocol)
			  ifTrue: [ { (EpProtocolRemoval behavior: behavior protocol: aChange protocol) } ]
			  ifFalse: [ #(  ) ] ].

	^ #(  )
]

{ #category : 'visitor' }
EpApplyPreviewer >> visitTraitChange: aChange [

	self
		behaviorNamed: aChange behaviorAffectedName
		ifPresent: [ :aTrait |
			^ { EpTraitModification oldTrait: aTrait newTrait: aChange behaviorAffected } ].

	^ { EpTraitAddition trait: aChange behaviorAffected }
]

{ #category : 'visitor' }
EpApplyPreviewer >> visitTraitRemoval: aChange [

	self
		behaviorNamed: aChange behaviorAffectedName
		ifPresent: [ :behavior | ^ { EpTraitRemoval trait: behavior } ].

	^ #()
]
