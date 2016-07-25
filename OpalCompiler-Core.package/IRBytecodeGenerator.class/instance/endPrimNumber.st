results
endPrimNumber
	"primitive number including quick return. Only valid at the end of generation"
	^ primNumber > 0
			ifTrue: [ primNumber ]
			ifFalse: [ self quickMethodPrim ]