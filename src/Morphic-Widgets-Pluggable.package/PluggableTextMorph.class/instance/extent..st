geometry
extent: newExtent
	"The inner bounds may have changed due to scrollbar visibility."

	super extent: (newExtent max: 36 @ 16).
	textMorph
		ifNotNil: [ 
			self innerExtent = textMorph extent
				ifFalse: [ textMorph extent: self innerExtent ] ].
	self setScrollDeltas.
	(self fillStyle notNil and: [ self fillStyle isSolidFill not ])
		ifTrue: [ self fillStyle: self fillStyleToUse ]