slices
metaclassExtensionSlicesDo: aBlock
	"This method iterates over the metaclass extensions and their associated selectors. A slice is a class * list of selectors. aBlock will be applied to all the extensions slices of the receiver. aBlok first argument is the class and second argument a list of selectors"
	
	metaclassExtensionSelectors keysAndValuesDo: aBlock
	
