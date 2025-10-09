Extension { #name : 'ClySystemBrowserContext' }

{ #category : '*Calypso-SystemPlugins-Reflectivity-Browser' }
ClySystemBrowserContext >> isSelectedItemHasBreakpoint [
	^selectedItems anySatisfy: [ :each |
		each isMarkedWith: ClyMethodWithBreakpointTag ]
]
