*Iceberg-Core
icePackage
	^ self isExtensionMethod 
		ifTrue: [ Iceberg packageForCategoryNamed: (self category withoutPrefix: '*') ]
		ifFalse: [ self iceClass package ] 