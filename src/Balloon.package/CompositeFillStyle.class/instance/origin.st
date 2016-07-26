accessing
origin
	"Answer an effective origin of any oriented fill styles.
	Answer the top-left minima."

	|origin|
	origin := nil.
	self fillStyles reverseDo: [:fs |
		fs isOrientedFill ifTrue: [
			origin := origin
				ifNil: [fs origin]
				ifNotNil: [origin min: fs origin]]].
	^origin ifNil: [0@0] "just in case"