private
setStream: aStream delimiter: aString

	stream := aStream.
	line := stream nextLine.
	self delimiter: aString.	"sets position"
