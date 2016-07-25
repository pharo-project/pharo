accessing
next: count putAll: collection startingAt: offset
	self writeChunkSize: count.
	stream next: count putAll: collection startingAt: offset.
	self crlf.
	chunkCount := chunkCount + 1.
	totalSize := totalSize + count
