testing-henry
testHenry012
	"Need to get creative to include the null character..."
	self runRegex: #('a.*c'	
		'axyzc' true (1 'axyzc')
		'axy zc' true (1 'axy zc') "testing that a dot matches a space"
		), (Array with: 'axy', (String with: 0 asCharacter), 'zc'), #(false nil "testing that a dot does not match a null"
		'axyzd' false nil)