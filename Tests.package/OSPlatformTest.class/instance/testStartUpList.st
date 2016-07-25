testing
testStartUpList
	"This test documents issue http://code.google.com/p/pharo/issues/detail?id=838"
	| startupList |
	
	startupList := (SessionManager default startupList collect: #handledId).
	
	self should: [ (startupList indexOf: #OSPlatform) < (startupList indexOf: #InputEventSensor) ]