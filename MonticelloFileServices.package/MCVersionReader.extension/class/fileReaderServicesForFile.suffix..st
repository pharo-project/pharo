*MonticelloFileServices
fileReaderServicesForFile: fullName suffix: suffix
	<fileService>
	self subclassesDo:[:class |
	^ ((suffix = class extension) or: [ suffix = '*' ])
		ifTrue: [class services]
		ifFalse: [Array new: 0]]
		