protocol
removeAllFromMethodsIconsCache: aMethod
	self methodsIconsCache
		keysDo: [ :method | 
			" for overrides "
			method selector = aMethod selector
				ifTrue: [ MethodsIconsCache removeKey: method ifAbsent: [  ] ] ]