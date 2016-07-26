gc
tenuringThreshold: anInteger
	"survivor count tenuring threshold (read-write).
	tenure when more than this many objects survive the GC"
	
	^ self parameterAt: 6 put: anInteger