tests
testBasenameWithoutExtension
	"self debug: #testBasenameWithoutExtension"
	
	| ref |
	
	ref := filesystem root.
	self assert: ref basename equals: '/'.
	
	ref := filesystem * 'plonk' / 'griffle'.
	self assert: ref basenameWithoutExtension equals: 'griffle'.
	self assert: (ref basenameWithoutExtension: 'griffle') equals: 'griffle'.
	self assert: (ref basenameWithoutExtension: 'taz') equals: 'griffle'.
	
	ref := filesystem * 'plonk' / 'griffle.taz'.
	self assert: ref basenameWithoutExtension equals: 'griffle'.
	self assert: (ref basenameWithoutExtension: 'taz') equals: 'griffle'.
	self assert: (ref basenameWithoutExtension: 'griffle.taz') equals: 'griffle.taz'.
	self assert: (ref basenameWithoutExtension: 'zork') equals: 'griffle.taz'.
	
	ref := filesystem * 'plonk' / 'griffle.taz.zork'.
	self assert: ref basenameWithoutExtension equals: 'griffle.taz'.
	self assert: (ref basenameWithoutExtension: 'zork') equals: 'griffle.taz'.
	self assert: (ref basenameWithoutExtension: 'taz.zork') equals: 'griffle'.
	self assert: (ref basenameWithoutExtension: 'griffle.taz.zork') equals: 'griffle.taz.zork'.
	self assert: (ref basenameWithoutExtension: 'taz') equals: 'griffle.taz.zork'.