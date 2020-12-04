Extension { #name : #MethodAnnouncement }

{ #category : #'*Calypso-SystemQueries' }
MethodAnnouncement >> canAffectResultOfMethodQuery: aMethodQuery [

	^aMethodQuery isAffectedByChangedMethod: self methodAffected
]
