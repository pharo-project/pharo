Extension { #name : #ProtocolAnnouncement }

{ #category : #'*Calypso-SystemQueries' }
ProtocolAnnouncement >> affectsMethodTagIn: aClass [

	^self classAffected == aClass
]
