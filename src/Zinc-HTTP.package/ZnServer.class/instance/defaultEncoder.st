options
defaultEncoder
	"The default character encoder to use when none is set in a mime-type"
	
	^ self optionAt: #defaultEncoder ifAbsent: [ ZnDefaultCharacterEncoder value ] 