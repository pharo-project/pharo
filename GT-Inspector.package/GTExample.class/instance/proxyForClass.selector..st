private
proxyForClass: aClass selector: aSelector
	" we have to resolve the kind of example again - it could be implemented in #class or in the provider of #class ! "
	^ self proxyForProvider: aClass gtExamplesProvider selector: aSelector