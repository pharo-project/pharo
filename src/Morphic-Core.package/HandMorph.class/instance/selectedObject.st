selected object
selectedObject
	"answer the selected object for the hand or nil is none"
	| halo |
	halo := self halo.
	halo ifNil: [^ nil].
	^ halo target renderedMorph