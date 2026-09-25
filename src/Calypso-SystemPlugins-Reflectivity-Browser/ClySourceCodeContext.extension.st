Extension { #name : 'ClySourceCodeContext' }

{ #category : '*Calypso-SystemPlugins-Reflectivity-Browser' }
ClySourceCodeContext >> isSelectedItemHasBreakpoint [

	^ self selectedSourceNode hasBreakpoint
]
