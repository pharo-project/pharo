drawing
refreshWorld
	| aWorld |
	(aWorld := self world) ifNotNil: [aWorld displayWorldSafely]
