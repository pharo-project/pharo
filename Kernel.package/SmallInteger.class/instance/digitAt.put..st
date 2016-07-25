system primitives
digitAt: n put: value 
	"Fails. The digits of a small integer can not be modified."

	self error: 'You can''t store in a SmallInteger'