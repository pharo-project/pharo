private
cachePackageAt: aString ifAbsentPut: aBlock
	"self addCount: aString."
	^ self cachePackages 
		at: aString asSymbol
		ifAbsentPut: aBlock