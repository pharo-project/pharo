enumerating
withAllPresentations
	| result |
	result := OrderedCollection new.
	self withAllPanesAndPresentationsDo: [:each |
		(each isKindOf: GLMPresentation) ifTrue: [ result add: each ]].
	^ result