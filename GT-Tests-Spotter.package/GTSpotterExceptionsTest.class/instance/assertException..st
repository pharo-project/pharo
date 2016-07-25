private
assertException: anErrorClass
	self deny: self class exceptions isEmpty.
	self assert: (self class exceptions anySatisfy: [ :e | e class == anErrorClass ]).
	self assert: self class fatals isEmpty.