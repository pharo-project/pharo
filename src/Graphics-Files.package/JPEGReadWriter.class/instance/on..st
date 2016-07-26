private
on: aStream 
	super on: aStream.
	stream := JPEGReadStream on: stream upToEnd