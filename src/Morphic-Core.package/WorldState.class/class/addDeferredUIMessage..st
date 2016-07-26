deferred message
addDeferredUIMessage: aValuable
	"aValuable will be executed in the next UI rendering cycle"
	self deferredUIMessages nextPut: aValuable

