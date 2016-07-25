category
removeCategory: aCategory inClass: aClass
	| protocol |
	aCategory ifNil: [ ^ false ].
	self okToChange ifFalse: [ ^ false].
	protocol := aClass organization protocolNamed: aCategory.
	protocol isVirtualProtocol ifTrue:[^ false ].
	protocol isEmpty ifTrue: [ aClass removeProtocol: aCategory. ^ true ].
	
	^ self confirmAndRemoveCategory: aCategory inClass: aClass
	