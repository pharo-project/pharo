accessing
loaderPolicy

	loaderPolicy == nil ifTrue: [ loaderPolicy := MetacelloLoaderPolicy new ].
	^ loaderPolicy