tests
testDefault
	self deny: preferences exists description: 'The preference file should not exists by default.'.
	self deny: preferences parent exists description: 'The preference directory should not exists by default.'