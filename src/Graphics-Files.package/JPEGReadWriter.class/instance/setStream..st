public access
setStream: aStream 
	"Feed it in from an existing source"
	stream := JPEGReadStream on: aStream upToEnd