scripting
composite: aBlock
	| composite |
	composite := GLMCompositePresentation new.
	aBlock value: composite.
	^ self custom: composite