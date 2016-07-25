character shapes
alter: char formBlock: formBlock
	self characterFormAt: char 
		put: (formBlock value: (self characterFormAt: char))