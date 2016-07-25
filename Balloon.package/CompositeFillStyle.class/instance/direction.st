accessing
direction
	"Answer an effective direction of any oriented fill styles.
	Answer the bottom-right maxima."

	|dir|
	dir := nil.
	self fillStyles reverseDo: [:fs |
		fs isOrientedFill ifTrue: [
			dir := dir
				ifNil: [fs direction]
				ifNotNil: [dir max: fs direction]]].
	^dir ifNil: [0@0] "just in case"