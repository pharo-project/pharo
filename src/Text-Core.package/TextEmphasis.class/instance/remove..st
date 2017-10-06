combining
remove: anotherCodeOrEmphasis
	"remove anotherCodeOrEmphasis from me"
	emphasisCode := emphasisCode bitAnd: 
		((anotherCodeOrEmphasis value bitInvert) bitAnd: (self allBits))