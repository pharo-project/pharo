world state
sleep

	worldState canvas ifNil: [^ self  "already called (clean this up)"].
	Cursor normal show.	"restore the normal cursor"
	worldState canvas: nil.		"free my canvas to save space"
	self fullReleaseCachedState.
