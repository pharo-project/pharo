private
copy: sourcePath ifAbsent: absentBlock to: destinationPath ifPresent: presentBlock fileSystem: aFilesystem

	| buffer out in |
	
	in := nil.
	out := nil.
	buffer := nil.
	[
		in := aFilesystem readStreamOn: sourcePath.
		in ifNil: [ ^ absentBlock value ].
		
		(self exists: destinationPath)
			ifTrue: [ "cannot overwrite destination"
				^ presentBlock value ].
			
		out := aFilesystem writeStreamOn: destinationPath.
		buffer := ByteArray new: 1024.
		
		[ in atEnd ]
			whileFalse: [ 
				buffer := in nextInto: buffer.
				out nextPutAll: buffer ]]
	ensure: [ 
		in ifNotNil: [ in close ].
		out ifNotNil: [ out close ]]