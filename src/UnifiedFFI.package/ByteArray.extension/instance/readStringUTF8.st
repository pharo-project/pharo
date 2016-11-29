*UnifiedFFI
readStringUTF8
	^ (ExternalData fromHandle: self type: ExternalType string) readStringUTF8.