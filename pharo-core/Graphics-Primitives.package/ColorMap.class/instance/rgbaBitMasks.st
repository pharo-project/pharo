accessing
rgbaBitMasks
	"Return the rgba bit masks for the receiver"
	^masks asArray with: shifts collect:[:m :s| m bitShift: s]