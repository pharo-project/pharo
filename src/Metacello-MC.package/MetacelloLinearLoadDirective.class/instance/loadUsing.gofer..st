actions
loadUsing: aLoaderDirective gofer: aGofer

	self loadDirectives isEmpty ifTrue: [ ^self ].
	aLoaderDirective loadLinearLoadDirective: self gofer: aGofer.