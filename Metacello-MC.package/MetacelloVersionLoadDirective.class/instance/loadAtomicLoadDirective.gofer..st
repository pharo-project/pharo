actions
loadAtomicLoadDirective: aLoaderDirective gofer: aGofer

	aLoaderDirective loadDirectives do: [:directive | directive loadUsing: aLoaderDirective gofer: aGofer ].
	aLoaderDirective finalizeLoad: aGofer.