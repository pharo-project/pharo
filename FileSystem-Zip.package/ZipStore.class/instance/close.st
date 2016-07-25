public
close
	| archive fs stream |
	archive := ZipArchive new.
	fs := FileSystem store: self.
	fs root allChildren do: 
		[ :each | 
		each isFile ifTrue: 
			[ each readStreamDo: 
				[ :output | 
				archive addString: output contents as: each path printString ] ] ].
	archive
		writeTo: (stream := WriteStream on: ByteArray new);
		close.
	self reference writeStreamDo: 
		[ :output | output nextPutAll: stream contents ]