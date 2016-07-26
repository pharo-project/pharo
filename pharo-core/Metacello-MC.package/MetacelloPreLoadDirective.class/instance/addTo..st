actions
addTo: aLoaderDirective

	spec preLoadDoIt value ~~ nil ifTrue: [ aLoaderDirective add: self ]