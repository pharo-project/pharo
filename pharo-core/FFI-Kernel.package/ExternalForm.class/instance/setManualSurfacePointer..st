initialize
setManualSurfacePointer: newPointer "ExternalStructure, ExternalAddress, or nil"
	"Set the memory-location of the image data.  It is OK to set a NULL pointer;
	 in this case, any attempt to BitBlt to or from the form will result in a primitive-failure."
	pointer := newPointer.
	pointer ifNil: [^self primManualSurface: bits setPointer: 0].
	pointer isExternalAddress
		ifFalse: ["must already be ExternalStructure, so nothing to do"]
		ifTrue: [pointer := ExternalData 
							fromHandle: newPointer 
							type: ExternalType void asPointerType].
	"The primitive expects an unsigned integer arg, not an ExternalAddress."
	"NOTE: it used to be acceptable for 'newPointer' to be an Integer... 
	 if you get a MNU for #getHandle here, you should update your code 
	 to pass in either an ExternalStructure or an ExternalAddress."
	self primManualSurface: bits setPointer: pointer getHandle asInteger