Extension { #name : 'PackageTagAnnouncement' }

{ #category : '*Calypso-SystemQueries' }
PackageTagAnnouncement >> affectsPackage: aPackage [

	^ self package == aPackage
]

{ #category : '*Calypso-SystemQueries' }
PackageTagAnnouncement >> affectsPackages [

	^ true
]
