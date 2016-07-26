responses
bytes: request
	"Answer a number of bytes according to a pattern as an application/octet-stream.
	Optionally take the size from the URI, as in /bytes/32 for 32 bytes, default to 64 bytes.
	The main goal is to be efficient, esp. in benchmarks, that's why we cache (non-thread-safe)"
	
	| count |
	count := Integer readFrom: request uri lastPathSegment ifFail: [ 64 ].
	(byteArray isNil or: [ byteArray size ~= count ]) 
		ifTrue: [
			byteArray := ByteArray new: count.
			1 to: count - 1 do: [ :each | 
				byteArray at: each put: (#(65 66 67 68) at: (each \\ 4 + 1)) ].
			byteArray at: count put: 10 ].
	^ ZnResponse ok: (ZnEntity bytes: byteArray)