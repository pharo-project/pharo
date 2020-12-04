Extension { #name : #SystemNavigation }

{ #category : #'*Deprecated90' }
SystemNavigation >> allSendersToString: aString do: aBlock [
	self 
		deprecated: 'use allReferencesTo:do: instead'  
		transformWith: '`@receiver allSendersToString: `@arg1 do: `@arg2' 
					-> '`@receiver allReferencesTo: `@arg1 do: `@arg2'.

	^ self allReferencesTo: aString asSymbol do: aBlock
]
