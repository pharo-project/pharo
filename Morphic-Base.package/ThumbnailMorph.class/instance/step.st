stepping and presenter
step
	"Optimization: Don't redraw if we're viewing some kind of SketchMorph and its rotated Form hasn't changed."

	| viewee |
	viewee := self actualViewee.
	viewee ifNil: [ self stopStepping. ^self ].
	self changed.
