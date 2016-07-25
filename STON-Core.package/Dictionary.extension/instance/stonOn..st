*ston-core
stonOn: stonWriter
	"Instances of STON mapClass will be encoded directly, without a class tag.
	Other (sub)classes will be encoded with a class tag and will use a map representation. "
	
	self class == STON mapClass
		ifTrue: [ 
			stonWriter writeMap: self ]
		ifFalse: [ 
			stonWriter 
				writeObject: self 
				do: [ stonWriter encodeMap: self ] ]