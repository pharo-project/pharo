Extension { #name : 'RPackageSet' }

{ #category : '*Deprecated12' }
RPackageSet >> unregister [

	self deprecated: 'Tihs method will be removed because the name does not conform with the naming convensions of this class.'.

	self packages do: #unregister
]
