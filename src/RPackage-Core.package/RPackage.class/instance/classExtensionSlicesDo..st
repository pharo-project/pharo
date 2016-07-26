slices
classExtensionSlicesDo: aBlock
	"This method iterates over the class extensions and their associated selectors. A slice is a class * list of selectors. aBlock will be applied to all the extensions slices of the receiver. aBlock first argument is the class and the second argument a list of method selectors"
	
	classExtensionSelectors keysAndValuesDo: aBlock