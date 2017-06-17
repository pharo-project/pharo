*Iceberg-Core
icePackage
	^ self isExtensionMethod 
		ifTrue: [ Iceberg packageForCategoryNamed: (self protocol withoutPrefix: '*') ]
		ifFalse: [ self iceClass package ] 