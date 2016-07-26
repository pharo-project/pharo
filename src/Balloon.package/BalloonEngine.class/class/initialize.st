initialization
initialize
	"BalloonEngine initialize"
	BufferCache := WeakArray new: 1.
	Smalltalk garbageCollect. "Make the cache old"
	CacheProtect := Semaphore forMutualExclusion.
	Times := WordArray new: 10.
	Counts := WordArray new: 10.
	BezierStats := WordArray new: 4.
	Debug ifNil:[Debug := false].