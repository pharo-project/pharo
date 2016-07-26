accessing
actualClass
	"Answer the class represented in the receiver."

	^(self operation ifNil: [^nil]) targetClass