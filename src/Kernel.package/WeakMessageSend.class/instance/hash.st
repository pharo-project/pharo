comparing
hash
	"work like MessageSend>>hash"
	^self receiver hash bitXor: selector hash
