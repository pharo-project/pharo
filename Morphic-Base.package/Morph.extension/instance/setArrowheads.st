*Morphic-Base
setArrowheads
	"Let the user edit the size of arrowheads for this object"

	| aParameter result  |
	aParameter := self renderedMorph valueOfProperty:  #arrowSpec ifAbsent: [PolygonMorph defaultArrowSpec].
	result := Morph obtainArrowheadFor: 'Head size for arrowheads: ' translated defaultValue: aParameter asString.
	result ifNotNil:
			[self renderedMorph  setProperty: #arrowSpec toValue: result]
		ifNil:
			[ self inform: 'Invalid input']