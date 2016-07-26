test shadowing
testNoShadowing
	interactive := true.
	self initializeErrorMessage.
	text := 'temp |var2|  var2:=1'.

	self compileWithFailBlock: [
		self fail.
		^nil ].
	

