Extension { #name : #ClySystemBrowserContext }

{ #category : #'*Calypso-SystemPlugins-Reflectivity-Browser' }
ClySystemBrowserContext >> isSelectedItemHasBreakpoint [
	^selectedItems anySatisfy: [ :each | 
		each isMarkedWith: ClyMethodWithBreakpointTag ]
]

{ #category : #'*Calypso-SystemPlugins-Reflectivity-Browser' }
ClySystemBrowserContext >> isSelectedItemHasExecutionCounter [
	^selectedItems anySatisfy: [ :each | 
		each isMarkedWith: ClyMethodWithExecutionCounterTag ]
]

{ #category : #'*Calypso-SystemPlugins-Reflectivity-Browser' }
ClySystemBrowserContext >> isSelectedItemHasWatch [
	^selectedItems anySatisfy: [ :each | 
		each isMarkedWith: ClyMethodWithWatchTag ]
]
