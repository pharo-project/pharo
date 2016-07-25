accessing
zip
	stream closed ifTrue: [ zip := nil].
	zip ifNil:
		[zip := ZipArchive new.
		zip readFrom: self stream].
	^ zip