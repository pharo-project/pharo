accessing
previewSize
	"Answer the size of preview to use."

	self previewType == #text ifTrue: [^256@256].
	^128@128