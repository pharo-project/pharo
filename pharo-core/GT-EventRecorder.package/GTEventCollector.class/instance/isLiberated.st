testing
isLiberated
	"When I answer true, I can be removed from a GTEventRecorder object."
	^ self registry isLiberated and: [ recordedEvents isEmpty ]