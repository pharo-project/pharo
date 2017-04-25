printing
printString
	"Return a String with my official representation, 32 lowercase hexadecimal digits, displayed in five groups separated by hyphens, in the form 8-4-4-4-12 for a total of 36 characters (32 alphanumeric characters and four hyphens)"

	^ String 
		new: 36 
		streamContents: [ :stringStream | self printOn: stringStream ]