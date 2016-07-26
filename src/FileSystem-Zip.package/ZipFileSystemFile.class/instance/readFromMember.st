private
readFromMember
	^ ByteArray 
		new: self member uncompressedSize
		streamContents: [ :stream |		
			self member extractTo: stream ]