visual properties
fillStyles
	"Answer all the fill styles"
	
	|styles|
	styles := OrderedCollection new.
	self stateMap do: [:actives |
		actives do: [:enableds |
			enableds do: [:overs |
				overs do: [:fs | styles add: fs]]]].
	^styles