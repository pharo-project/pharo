Registered file services for specific file types.

To use me you can register a class by adding a method as: 

MyClass class>>fileReaderServicesForFile: fullName suffix: suffix
	<fileService>
	
	^ (FileStream isSourceFileSuffix: suffix)
		ifTrue: [ { self mySimpleServiceEntry1 . self mySimpleServiceEntry2 }]
		ifFalse: [#()]