convenience
convert: byteString
	"Convert byteString to a wideString, signalling a resumable error"
	
	| wideString |
	wideString := WideString from: byteString.
	self new
		byteString: byteString;
		wideString: wideString;
		signal.
	^ wideString