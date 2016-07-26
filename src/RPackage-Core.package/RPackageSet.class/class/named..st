instance creation
named: aString

	^ self isCacheActive 
		ifTrue: [ 
			self 
				cachePackageAt: aString 
				ifAbsentPut: [ self basicNamed: aString ] ]
		ifFalse: [ self basicNamed: aString ].
