enumerating
withAllPanes
	| result |
	result := OrderedCollection new.
	self withAllPanesAndPresentationsDo: [:each |
		(each isKindOf: GLMPane) ifTrue: [ result add: each ]].
	^ result