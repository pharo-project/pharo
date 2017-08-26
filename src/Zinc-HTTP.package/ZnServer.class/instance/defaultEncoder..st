options
defaultEncoder: encoder
	"Set the default character encoder to use when none is set in a mime-type"
	
	^ self optionAt: #defaultEncoder put: encoder asZnCharacterEncoder