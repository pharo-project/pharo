*compression
zipped
	| stream gzstream |

	stream := RWBinaryOrTextStream on: String new.

	gzstream := GZipWriteStream on: stream.
	gzstream nextPutAll: self.
	gzstream close.
	stream reset.

	^ stream contents.
