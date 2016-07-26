filein/out
postscript
	"Answer the string representing the postscript.  "
	^postscript ifNotNil:[postscript isString ifTrue:[postscript] ifFalse:[postscript contents asString]]