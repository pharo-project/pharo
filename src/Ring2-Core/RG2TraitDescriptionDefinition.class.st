"
RG2TraitDescriptionDefinition is the common parent for traits and metatraits that know their users (ie. classes/metaclasses)
"
Class {
	#name : #RG2TraitDescriptionDefinition,
	#superclass : #RG2BehaviorDefinition,
	#category : #'Ring2-Core-Kernel'
}

{ #category : #variables }
RG2TraitDescriptionDefinition >> allSharedPoolNames [

	^#()
]

{ #category : #initialization }
RG2TraitDescriptionDefinition >> initialize [

	super initialize.
	
	self environment backend createUnresolvedTraitGroupFor: self.
	
]

{ #category : #testing }
RG2TraitDescriptionDefinition >> isTrait [

	^true
]

{ #category : #'queries - methods' }
RG2TraitDescriptionDefinition >> methods [

	| allMethods |
	allMethods := IdentitySet new.
	self localMethodsDo: [ :each | allMethods add: each].
	^ allMethods asArray

]

{ #category : #'as yet unclassified' }
RG2TraitDescriptionDefinition >> name: aString [

	| usersWithOldDefinition |

	usersWithOldDefinition := self users collect: [ :each | 
		each -> each copyForBehaviorDefinition ].

	super name: aString.
	
	usersWithOldDefinition do: [ :assoc |
		self announcer behaviorDefinitionChangedFrom: assoc value to: assoc key.
		self announcer behaviorModificationAppliedTo: assoc key. ].		
]

{ #category : #accessing }
RG2TraitDescriptionDefinition >> sharedPools [

	^#()
]

{ #category : #'as yet unclassified' }
RG2TraitDescriptionDefinition >> trait [

	^ self
]

{ #category : #'as yet unclassified' }
RG2TraitDescriptionDefinition >> users [
	
	"return collection of user of the trait"

	| result |
	
	result := IdentitySet new.

	self environment ask behaviorsDo: [ :each |
		(each usedTraits includes: self) ifTrue: [ result add: each ] ].
	
	^ result.
]
