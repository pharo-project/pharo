Extension { #name : 'OCProgramNode' }

{ #category : '*Calypso-SystemPlugins-DebugPoints-Browser' }
OCProgramNode >> hasDebugPointOfType: aDebugPointSubClass [
	
	^self links anySatisfy: [ :link | link metaObject isKindOf: aDebugPointSubClass ]
]
