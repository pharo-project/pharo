*System-FileRegistry
fileReaderServicesForFile: fullName suffix: suffix 
	<fileService>
	"Answers a collection of services that can handle the file and suffixes"
	|  services |
	services := OrderedCollection new.
	({'zip'.'sar'.'pr'. 'mcz'. '*'} includes: suffix)
		ifTrue: [	services add: self serviceExtractAll].
	^services.