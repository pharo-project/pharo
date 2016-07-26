strategy
standardFor: aView initialExtent: initialExtent world: aWorld

	| allOrigins screenRight screenBottom putativeOrigin putativeFrame allowedArea staggerOrigin otherFrames |
	allowedArea := self maximumUsableAreaInWorld: aWorld.
	screenRight := allowedArea right.
	screenBottom := allowedArea bottom.
	otherFrames := (aWorld
				windowsSatisfying: [:w | w isCollapsed not])
				collect: [:w | w bounds].
	allOrigins := otherFrames
				collect: [:f | f origin].
	(self standardPositionsInWorld: aWorld)
		do: [:aPosition | "First see if one of the standard positions is free"
			(allOrigins includes: aPosition)
				ifFalse: ["First see if one of the standard positions is free"
					^ (aPosition extent: initialExtent)
						translatedAndSquishedToBeWithin: allowedArea"First see if one of the standard positions is free"]].
	staggerOrigin := (self standardPositionsInWorld: aWorld) first.
	"Fallback: try offsetting from top left"
	putativeOrigin := staggerOrigin.
	[putativeOrigin := putativeOrigin + StaggerOffset.
	putativeFrame := putativeOrigin extent: initialExtent.
	putativeFrame bottom < screenBottom
		and: [putativeFrame right < screenRight]]
		whileTrue: [(allOrigins includes: putativeOrigin)
				ifFalse: [^ (putativeOrigin extent: initialExtent)
						translatedAndSquishedToBeWithin: allowedArea]].
	^ (self scrollBarSetback @ self screenTopSetback extent: initialExtent)
		translatedAndSquishedToBeWithin: allowedArea