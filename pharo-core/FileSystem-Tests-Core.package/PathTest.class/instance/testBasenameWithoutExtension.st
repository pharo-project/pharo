tests
testBasenameWithoutExtension
	"self debug: #testBasenameWithoutExtension"
	| path |
	path := Path * 'plonk' / 'griffle'.
	self assert: path basenameWithoutExtension equals: 'griffle'.
	self assert: (path basenameWithoutExtension: 'griffle') equals: 'griffle'.
	self assert: (path basenameWithoutExtension: 'taz') equals: 'griffle'.
	
	path := Path * 'plonk' / 'griffle.taz'.
	self assert: path basenameWithoutExtension equals: 'griffle'.
	self assert: (path basenameWithoutExtension: 'taz') equals: 'griffle'.
	self assert: (path basenameWithoutExtension: 'griffle.taz') equals: 'griffle.taz'.
	self assert: (path basenameWithoutExtension: 'zork') equals: 'griffle.taz'.
	
	path := Path * 'plonk' / 'griffle.taz.zork'.
	self assert: path basenameWithoutExtension equals: 'griffle.taz'.
	self assert: (path basenameWithoutExtension: 'zork') equals: 'griffle.taz'.
	self assert: (path basenameWithoutExtension: 'taz.zork') equals: 'griffle'.
	self assert: (path basenameWithoutExtension: 'girffle.taz.zork') equals: 'griffle.taz.zork'.
	self assert: (path basenameWithoutExtension: 'taz') equals: 'griffle.taz.zork'.