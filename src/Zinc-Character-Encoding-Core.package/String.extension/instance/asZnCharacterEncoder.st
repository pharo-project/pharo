*Zinc-Character-Encoding-Core
asZnCharacterEncoder
	"Return a ZnCharacterEncoder instance using the receiver as identifier"
	
	" 'UTF-8' asZnCharacterEncoder "
	
	^ ZnCharacterEncoder newForEncoding: self