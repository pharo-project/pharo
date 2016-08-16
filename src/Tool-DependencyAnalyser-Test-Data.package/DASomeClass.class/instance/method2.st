tests - test data
method2
	"a message send not in Kernel package"
	(1 to: 3) collect: [ :i | i * 2 ] as: #Array asClass "avoid a direct reference to Array"