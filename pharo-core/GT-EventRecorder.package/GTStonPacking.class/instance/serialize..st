private
serialize: anObject
	^ (String streamContents: [ :stream | | gzstream |
		gzstream := GZipWriteStream on: stream.
		[ STON put: anObject onStreamPretty: gzstream ]
			ensure: [ gzstream close ] ]) asByteArray