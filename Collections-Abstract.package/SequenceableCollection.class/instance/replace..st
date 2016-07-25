enumerating
replace: aBlock 
	"Evaluate aBlock with each of the receiver's elements as the argument.  
	Collect the resulting values into self."

	1 to: self size do: [ :index |
		self at: index put: (aBlock value: (self at: index)) ]