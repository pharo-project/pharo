halo handling
removeHalo
	"remove the receiver's halo (if any)"
	self halo ifNotNil: [ :h | self removeHaloAround: h target ]