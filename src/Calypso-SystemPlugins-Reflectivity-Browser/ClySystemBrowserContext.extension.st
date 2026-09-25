Extension { #name : 'ClySystemBrowserContext' }

{ #category : '*Calypso-SystemPlugins-Reflectivity-Browser' }
ClySystemBrowserContext >> isSelectedItemHasBreakpoint [

	^ self selectedItems anySatisfy: [ :each | each isMarkedWith: ClyMethodWithBreakpointTag ]
]
