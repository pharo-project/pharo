actions
loadUsing: aLoaderDirective gofer: aGofer

	self loadDirectives isEmpty ifTrue: [ ^self ].
	aLoaderDirective loadAtomicLoadDirective: self gofer: aGofer.