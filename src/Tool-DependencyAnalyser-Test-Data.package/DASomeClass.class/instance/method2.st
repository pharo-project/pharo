tests - test data
method2
	"a message send not in Kernel package"
	(1 to: 3) 
		collect: [ :i | i * 2 ] 
		as: (self class environment at: #Array) "avoid a direct reference to Array"