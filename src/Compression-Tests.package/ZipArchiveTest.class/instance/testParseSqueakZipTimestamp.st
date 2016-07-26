tests
testParseSqueakZipTimestamp
	"Issue 6054. We're testing a private method. Eventually, the zip timestamp handling should be refactored; maybe a ZipTimestamp subclass of DosTimestamp"

	| squeakDosTimestamp dateAndTime |
	"This was the error-causing timestamp from http://code.google.com/p/pharo/issues/detail?id=6054#c1 . It was the 'package' file inside Balloon-CamilloBruni.85.mcz"
	squeakDosTimestamp := 1001728866.
	dateAndTime := ZipFileMember new unzipTimestamp: squeakDosTimestamp.
	self assert: dateAndTime equals: '2011-09-29T04:01:06+00:00' asDateAndTime.