Read a Date from the stream based on the pattern which can include the tokens:
	
		y = A year with 1 to n digits (after 2000)
		yy = A year with 2 digits (after 2000)
		yyyy = A year with 4 digits
		m = A month with 1 or 2 digits
		mm = A month with 2 digits
		d = A day with 1 or 2 digits
		dd = A day with 2 digits
		
	...and any other Strings inbetween. Representing $y, $m and $d is done using
	\y, \m and \d and slash itself with \\. Simple example patterns:

		'yyyy-mm-dd'
		'yyyymmdd'
		'yy.mm.dd'
		'y-m-d'