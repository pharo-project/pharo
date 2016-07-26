accessing
sizeBuffer: size
	buffer := (stream isBinary ifTrue: [ ByteArray ] ifFalse: [ String ]) new: size