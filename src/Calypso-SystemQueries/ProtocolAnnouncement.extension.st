Extension { #name : 'ProtocolAnnouncement' }

{ #category : '*Calypso-SystemQueries' }
ProtocolAnnouncement >> affectsProtocolsIn: aClass [

	^ self classAffected == aClass
]
