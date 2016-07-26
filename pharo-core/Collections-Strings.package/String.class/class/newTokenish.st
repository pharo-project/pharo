initialization
newTokenish
	"a table for testing tokenish (for fast numArgs)"
	^ String withAll: (Character allByteCharacters 
		collect: [:c | c tokenish ifTrue: [ c ] ifFalse: [ $~ ]]).