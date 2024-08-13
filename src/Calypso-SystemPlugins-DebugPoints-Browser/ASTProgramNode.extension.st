Extension { #name : 'ASTProgramNode' }

{ #category : '*Calypso-SystemPlugins-DebugPoints-Browser' }
ASTProgramNode >> hasDebugPointOfType: aDebugPointSubClass [
	
	^self links anySatisfy: [ :link | link metaObject isKindOf: aDebugPointSubClass ]
]
