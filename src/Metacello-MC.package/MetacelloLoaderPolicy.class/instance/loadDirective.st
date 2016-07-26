accessing
loadDirective

	loadDirective == nil ifTrue: [ loadDirective := MetacelloLinearLoadDirective new ].
	^ loadDirective