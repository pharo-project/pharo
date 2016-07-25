adding
addToCurrentLayer: aVersionInfo
	| layer |
	layer := layers at: depthIndex.
	(layer includes: aVersionInfo) ifFalse:
		[depths at: aVersionInfo ifPresent:
			[:i |
			i < depthIndex
				ifTrue: [(layers at: i) remove: aVersionInfo]
				ifFalse: [^ false]].
		layer add: aVersionInfo.
		depths at: aVersionInfo put: depthIndex.
		^ true].
	^ false 