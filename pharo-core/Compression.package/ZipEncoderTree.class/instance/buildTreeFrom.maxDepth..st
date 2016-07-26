encoding
buildTreeFrom: frequencies maxDepth: depth
	"Build the receiver from the given frequency values"
	| nodeList |
	nodeList := Array new: frequencies size.
	1 to: frequencies size do:[:i|
		nodeList at: i put: (ZipEncoderNode value: i-1 frequency: (frequencies at: i) height: 0)
	].
	self buildTree: nodeList maxDepth: depth.