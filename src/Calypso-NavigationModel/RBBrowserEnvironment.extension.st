Extension { #name : 'RBBrowserEnvironment' }

{ #category : '*Calypso-NavigationModel' }
RBBrowserEnvironment >> systemChanged: aSystemAnnouncement [

	self updateUsing: ClyFullEnvironmentUpdateStrategy new by: [
		self handleSystemChange: aSystemAnnouncement
	]
]

{ #category : '*Calypso-NavigationModel' }
RBBrowserEnvironment >> updateStrategy [
	^ updateStrategy ifNil: [ updateStrategy := ClyInstantEnvironmentUpdateStrategy new ]
]
