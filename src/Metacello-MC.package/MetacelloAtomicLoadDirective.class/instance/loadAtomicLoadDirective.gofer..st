actions
loadAtomicLoadDirective: aLoaderDirective gofer: aGofer

	aLoaderDirective loadDirectives do: [:directive | directive loadUsing: self gofer: aGofer ].