testing-henry
testHenry107
	self runRegex: #('(a|b|c|d|e)f'
		'ef' true (1 'ef' 2 'e'))
	"	((a*|b))*	-	c	-	-"