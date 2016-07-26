private
saveLastJump: message
	jumps at: currentSeqId put: {bytes size. message}.
 