operations
last
	| externalData |
	externalData := self giterr_last.
	^ self fromHandle: externalData getHandle