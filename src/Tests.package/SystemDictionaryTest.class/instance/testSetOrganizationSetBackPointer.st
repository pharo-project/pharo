tests
testSetOrganizationSetBackPointer
	"self debug: #testSetOrganizationSetBackPointer"
	
	| sd so |
	sd := SystemDictionary new.
	so := SystemOrganizer new.
	sd organization: so.
	self assert: so environment == sd.
	self assert: sd organization == so
	