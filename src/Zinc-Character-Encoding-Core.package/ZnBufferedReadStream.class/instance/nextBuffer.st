private
nextBuffer
	limit := stream readInto: buffer startingAt: 1 count: buffer size.
	position := 1