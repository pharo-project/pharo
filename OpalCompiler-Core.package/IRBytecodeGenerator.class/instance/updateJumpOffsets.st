private
updateJumpOffsets
	[ orderSeq 
		inject: false 
		into: [ :changed :seqId | (self updateJump: seqId) or: [ changed ]] 
	] whileTrue