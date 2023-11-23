Extension { #name : 'BreakpointAnnouncement' }

{ #category : '*Calypso-SystemPlugins-Reflectivity-Queries' }
BreakpointAnnouncement >> canAffectResultOfMethodQuery: aMethodQuery [

	^ self link methods anySatisfy: [:each |
		aMethodQuery isAffectedByChangedMethod: each ]
]
