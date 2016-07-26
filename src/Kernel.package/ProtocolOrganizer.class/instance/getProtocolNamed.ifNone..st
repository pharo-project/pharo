protocol
getProtocolNamed: aByteString ifNone: aBlockClosure 
	
	^ protocols detect: [:e | e name = aByteString ] ifNone: aBlockClosure 