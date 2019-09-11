Extension { #name : #ClassReorganized }

{ #category : #'*Calypso-SystemQueries' }
ClassReorganized >> shouldBeConsideredByCalypsoEnvironment [
	"this event is actually about protocol rename.
	So we skip it and use explicit ProtocolRenamed event"
	^false
]
