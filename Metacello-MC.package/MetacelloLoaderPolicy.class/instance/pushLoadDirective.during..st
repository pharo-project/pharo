private
pushLoadDirective: aLoaderDirective during: aBlock

	|  oldRoot |
	self loadDirective add: aLoaderDirective.
	oldRoot := loadDirective.
	loadDirective := aLoaderDirective.
	aBlock ensure: [ loadDirective := oldRoot ].