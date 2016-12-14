as yet unclassified
checkpoint: name

	| isNewSession |
	isNewSession := Smalltalk saveAs: name.
	isNewSession ifTrue: [ self halt ].
	self flag: #pharo_is_broken.
	"We do not do it, it is broken in latest pharo"