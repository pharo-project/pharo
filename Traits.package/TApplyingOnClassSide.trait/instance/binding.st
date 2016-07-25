compiling
binding
	"return an association that can be used as the binding
	 To share it between methods, reuse an existing one if possible"
	^self methodDict 
		ifEmpty: [nil -> self]
		ifNotEmpty: [:dict | dict anyOne classBinding]