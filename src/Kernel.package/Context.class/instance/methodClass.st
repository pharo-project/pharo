debugger access
methodClass 
	"Answer the class in which the receiver's method was found."
	
	^self method methodClass ifNil:[self receiver class].