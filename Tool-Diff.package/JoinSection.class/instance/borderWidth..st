accessing
borderWidth: anInteger
	"Set the value of borderWidth"

	borderWidth := anInteger.
	self src highlight notNil ifTrue: [
		self src highlight borderWidth: anInteger].
	self dst highlight notNil ifTrue: [
		self dst highlight borderWidth: anInteger]