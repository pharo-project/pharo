icon caches
removeClassFromMethodsIconsCache: aClass

	self methodsIconsCache keysDo: [:method | 
		" for overrides "
		method methodClass = aClass
			ifTrue: [ 
				self methodsIconsCache removeKey: method ifAbsent: [ ]]].