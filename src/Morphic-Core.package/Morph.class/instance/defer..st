deferred message
defer: aValuable
	"aValuable will be executed in the next UI rendering cycle"
	self owner 
		ifNotNil: [ self owner defer: aValuable] 
		ifNil: [ UIManager default defer: aValuable ]