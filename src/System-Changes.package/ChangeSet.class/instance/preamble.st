filein/out
preamble
	"Answer the string representing the preamble"
	^preamble ifNotNil:[preamble isString ifTrue:[preamble] ifFalse:[preamble contents asString]]