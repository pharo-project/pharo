Extension { #name : #ClySourceCodeContext }

{ #category : #'*Calypso-SystemPlugins-Reflectivity-Browser' }
ClySourceCodeContext >> isSelectedItemHasBreakpoint [
	^selectedSourceNode hasBreakpoint 
]

{ #category : #'*Calypso-SystemPlugins-Reflectivity-Browser' }
ClySourceCodeContext >> isSelectedItemHasExecutionCounter [
	^selectedSourceNode hasExecutionCounter 
]

{ #category : #'*Calypso-SystemPlugins-Reflectivity-Browser' }
ClySourceCodeContext >> isSelectedItemHasWatchpoint [
	^selectedSourceNode hasWatchpoint
]
