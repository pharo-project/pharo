spec classes
loaderClass

	loaderClass == nil ifTrue: [ loaderClass := self defaultLoaderClass ].
	^loaderClass