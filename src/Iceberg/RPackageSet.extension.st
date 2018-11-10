Extension { #name : #RPackageSet }

{ #category : #'*Iceberg' }
RPackageSet class >> withoutCacheDo: aBlock [ 
	| oldActive |
	
	oldActive := self isCacheActive.
	self cacheActive: false.
	aBlock ensure: [ self cacheActive: oldActive ].
]
