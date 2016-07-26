geometry
referencePosition
	"Return the current reference position of the receiver"
	| box |
	box := self bounds.
	^box origin + (self rotationCenter * box extent).
