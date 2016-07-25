accessing
font: aFont
	"Set the label font"

	self normalLabel allMorphsDo: [:m |
		(m respondsTo: #font:) ifTrue: [m font: aFont]].
	self selectedLabel allMorphsDo: [:m |
		(m respondsTo: #font:) ifTrue: [m font: aFont]]