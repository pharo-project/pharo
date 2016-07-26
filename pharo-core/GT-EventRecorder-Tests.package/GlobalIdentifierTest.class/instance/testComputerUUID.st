tests
testComputerUUID
	"The computer UUID is not stored immediately because the Pharo Integration Server
	raised building errors when storing the computer UUID on the CI server."
	self assert: identifier computerUUID notNil.
	self assert: identifier computerUUID isString.
	self testDefault.