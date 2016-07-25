layout
adjustLayoutBounds
	"Adjust the receivers bounds depending on the resizing strategy imposed"

	| hFit vFit box myExtent extent |
	hFit := self hResizing.
	vFit := self vResizing.
	(hFit == #shrinkWrap or: [ vFit == #shrinkWrap ])
		ifFalse: [ ^ self ].	"not needed"
	box := self layoutBounds.
	myExtent := box extent.
	extent := self submorphBounds corner - box origin.
	hFit == #shrinkWrap
		ifTrue: [ myExtent := extent x @ myExtent y ].
	vFit == #shrinkWrap
		ifTrue: [ myExtent := myExtent x @ extent y ].	"Make sure we don't get smaller than minWidth/minHeight"
	myExtent x < self minWidth
		ifTrue: [ myExtent := (myExtent x max: self minWidth - self bounds width + self layoutBounds width) @ myExtent y ].
	myExtent y < self minHeight
		ifTrue: [ myExtent := myExtent x @ (myExtent y max: self minHeight - self bounds height + self layoutBounds height) ].
	self layoutBounds: (box origin extent: myExtent)