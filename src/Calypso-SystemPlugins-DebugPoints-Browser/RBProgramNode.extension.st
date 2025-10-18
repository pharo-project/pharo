Extension { #name : 'RBProgramNode' }

{ #category : '*Calypso-SystemPlugins-DebugPoints-Browser' }
RBProgramNode >> hasDebugPointOfType: aDebugPointSubClass [
	
	^self links anySatisfy: [ :link | link metaObject isKindOf: aDebugPointSubClass ]
]
