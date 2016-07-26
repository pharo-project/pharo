accessing
change
	"an 'add method' change that create the missing method"
	
	^ RBAddMethodChange 
		compile: (self methodHeader), String cr, String tab, source
		in: class