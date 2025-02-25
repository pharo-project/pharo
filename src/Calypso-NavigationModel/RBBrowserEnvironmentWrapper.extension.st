Extension { #name : 'RBBrowserEnvironmentWrapper' }

{ #category : '*Calypso-NavigationModel' }
RBBrowserEnvironmentWrapper >> query: aQuery [

	self isBasedOnEmptyBasis ifTrue: [
		"It is optimization to prevent real query execution in remote scenario.
		Any query is a priory empty when its scope is based on empty basis"
		^ClyUnknownQueryResult instance ].

	aQuery ensureScope: self.

	^environment query: aQuery
]

{ #category : '*Calypso-NavigationModel' }
RBBrowserEnvironmentWrapper >> supportsQuery: aQuery [
	^ aQuery isKindOf: ClyTypedQuery
]
