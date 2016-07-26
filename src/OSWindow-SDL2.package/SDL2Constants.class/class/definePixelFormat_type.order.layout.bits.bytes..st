class initialization
definePixelFormat_type: type order: order layout: playout bits: bits bytes: bytes
	^ ((1 << 28) | ((type) << 24) | ((order) << 20) | ((playout) << 16) |
     	    ((bits) << 8) | ((bytes) << 0))