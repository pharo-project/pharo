*Slot
isPseudovariableName
	"Answer true if I am a pseudo-variable name.
	#self isPseudovariableName -> true
	"
	^ self class pseudovariablesNames includes: self