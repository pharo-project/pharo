system primitives
instVarAt: i 
	"Small integer has to be specially handled."

	i = 1 ifTrue: [^self].
	self error: 'argument too big for small integer instVarAt:'