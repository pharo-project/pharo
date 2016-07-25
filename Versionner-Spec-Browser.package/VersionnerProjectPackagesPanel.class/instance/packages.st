accessing
packages
	^ project ifNil: [ #() ] ifNotNil: [ project packages ] 