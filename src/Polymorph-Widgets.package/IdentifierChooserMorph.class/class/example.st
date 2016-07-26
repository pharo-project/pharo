instance creation
example

	(self 
		labels: #('aaa' 'bbb' 'ccc') 
		chooseBlock:  [ :currText | currText matches: '*bb'])
	openInWorld