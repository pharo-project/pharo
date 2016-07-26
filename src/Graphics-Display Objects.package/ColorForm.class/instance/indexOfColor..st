color manipulation
indexOfColor: aColor
	"Return the index of aColor in my color array"

	self ensureColorArrayExists.
	^ colors indexOf: aColor ifAbsent: [0]