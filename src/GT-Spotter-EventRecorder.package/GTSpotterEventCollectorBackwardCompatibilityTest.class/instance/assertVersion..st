asserting
assertVersion: aMethodName
	| announcement |
	announcement := unpacking unpack: (self perform: aMethodName).
	self assertAnnouncement: announcement.
	checkedEvents addAll: (announcement unpackedData collect: #class).