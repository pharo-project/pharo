tests
testUUIDs
	| uuids |
	uuids := STON listClass withAll: { 
		UUID new.
		UUID new.
		UUID nilUUID }.
	self serializeAndMaterialize: uuids