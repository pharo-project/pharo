accessing
spacerWidth
	"Such a morph composed of, left to right, some space according to the level in the tree, an expand toggle (if any), a check box (if any),  and the item morphs (icon + text often). Compute here the width of the space, including the toggle (and the check box) if there is one."

	| baseRect |
	baseRect := self mustTakeIntoAccountCheckSpace
		ifTrue: [ self checkRectangle ]
		ifFalse: [ self toggleRectangle ].
	^ (self mustTakeIntoAccountToggleSpace or: [ self mustTakeIntoAccountCheckSpace ])
		ifTrue: [ baseRect right + container gapAfterToggle - bounds left ]
		ifFalse: [ baseRect left - bounds left ]