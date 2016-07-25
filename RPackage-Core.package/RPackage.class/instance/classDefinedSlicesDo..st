slices
classDefinedSlicesDo: aBlock
	"This method iterates over the defined class and their associated selectors. a slice is a class * list of selectors. aBlock will be applied to all the extensions slices of the receiver. aBlok first argument is the class and the second argument a list of method selectors"
	
	classDefinedSelectors keysAndValuesDo: aBlock