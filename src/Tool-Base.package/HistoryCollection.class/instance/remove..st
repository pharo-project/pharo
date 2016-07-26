protocol
remove: anElement

	| association |
	association := self at: anElement.
	^ storage remove: association.
	