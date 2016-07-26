actions
addTo: aLoaderDirective

	spec postLoadDoIt value ~~ nil ifTrue: [ aLoaderDirective add: self ]