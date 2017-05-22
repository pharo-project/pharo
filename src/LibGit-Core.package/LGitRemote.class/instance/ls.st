accessing
ls
	"I list all HEADS in connected remotes (you will need to #connectFetch or 
	#connectPushWithCredentials: first)"
	| out sizeBuf heads size |
	
	sizeBuf := ByteArray new: FFISizeT externalTypeSize.
	out := ExternalAddress new.
	
	self withReturnHandlerDo: [ 
		self remote_ls: out size: sizeBuf ].
	
	size := sizeBuf signedLongAt: 1.
	heads := Array new: size. 
	1 to: size do: [ :index | 
		heads at: index put: (LGitRemoteHead fromHandle: (out pointerAt: 1)).
		out := out + FFIExternalType pointerSize ].
	
	^ heads