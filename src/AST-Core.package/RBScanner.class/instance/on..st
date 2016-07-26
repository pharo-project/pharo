initialize-release
on: aStream 
	buffer := (String new: 60) writeStream.
	stream := aStream.
	classificationTable := self class classificationTable.
	comments := OrderedCollection new