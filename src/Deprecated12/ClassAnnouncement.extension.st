Extension { #name : 'ClassAnnouncement' }

{ #category : '*Deprecated12' }
ClassAnnouncement >> classTagAffected [

	self deprecated: 'Use #packageTagAffected that returns a real package tag instance instead.'.
	^ self packagesAffected name
]
