cleanup
cleanUp: aggressive
	"Flush synthesized strike fonts"

	self allInstancesDo: [:sf| sf reset].