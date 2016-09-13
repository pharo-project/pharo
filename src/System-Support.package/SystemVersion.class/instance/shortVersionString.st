printing
shortVersionString
	"Print a short version string.  Compatibile with senders that concatenate this with 
	#lastUpdateString or #highestUpdate. Essentially the old version format. Guard 
	against no state being set."
	
	self type ifNil: [^'No version set'].
	^String streamContents: [:str |
		str 
			nextPutAll: self type;
			nextPutAll: self major printString;
			nextPut: $.;
			nextPutAll: self minor printString;
			nextPutAll: self suffix]