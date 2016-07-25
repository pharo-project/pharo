accessing
registry
	"Registry is a weak holder of my owner. Once it is nil, I can be removed from GTEventRecorder object."
	^ registry ifNil: [ registry := GTEventRegistry new ]