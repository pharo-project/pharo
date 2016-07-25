private
processVersionInfo: aVersionInfo
	| success |
	aVersionInfo = target ifTrue: [^ true].
	(aVersionInfo hasAncestor: target) ifFalse: [^false].
	self pushLayer.
	success := (self knownAncestorsOf: aVersionInfo) anySatisfy:
				[:ea | self processVersionInfo: ea].
	self popLayer.
	success ifTrue: [self addToCurrentLayer: aVersionInfo].
	^ success	