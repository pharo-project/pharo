Extension { #name : #ClassTagAnnouncement }

{ #category : #'*Calypso-SystemQueries' }
ClassTagAnnouncement >> affectsPackage: aPackage [
	^package == aPackage
]

{ #category : #'*Calypso-SystemQueries' }
ClassTagAnnouncement >> affectsPackages [
	^true
]
