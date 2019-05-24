Extension { #name : #Behavior }

{ #category : #'*Calypso-SystemQueries' }
Behavior >> calypsoEnvironmentType [
	^ClyClass
]

{ #category : #'*Calypso-SystemQueries' }
Behavior >> includesMethodsAffectedBy: aSystemAnnouncement [
	^aSystemAnnouncement affectsMethodsDefinedInClass: self 

]

{ #category : #'*Calypso-SystemQueries' }
Behavior >> metaLevelScope [
	^self isInstanceSide ifTrue: [ ClyInstanceSideScope ] ifFalse: [ ClyClassSideScope ]
]
