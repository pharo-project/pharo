*UnifiedFFI
writeString: aString
	LibC memCopy: aString to: self size: aString size