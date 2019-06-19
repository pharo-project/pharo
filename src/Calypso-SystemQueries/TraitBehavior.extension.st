Extension { #name : #TraitBehavior }

{ #category : #'*Calypso-SystemQueries' }
TraitBehavior >> calypsoEnvironmentType [
	^ClyClass
]

{ #category : #'*Calypso-SystemQueries' }
TraitBehavior >> includesMethodsAffectedBy: aSystemAnnouncement [
	^aSystemAnnouncement affectsMethodsDefinedInClass: self 

]

{ #category : #'*Calypso-SystemQueries' }
TraitBehavior >> metaLevelScope [
	^self isInstanceSide ifTrue: [ ClyInstanceSideScope ] ifFalse: [ ClyClassSideScope ]
]
