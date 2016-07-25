icon caches
removeFromMethodsIconsCache: aMethod
	" if a test method has changed, we should reset the correspondng method "
	
	| corresponding |
	corresponding := aMethod correspondingMethods.
	corresponding do: [:each | self methodsIconsCache removeKey: each ifAbsent: []].
	self methodsIconsCache removeKey: aMethod ifAbsent: [ ].