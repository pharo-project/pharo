tests
testGroupUpdatesByVersionLf
	"self debug: #testGroupUpdatesByVersionLf"
	
	| res |
	res := UpdateStreamer new groupUpdatesByVersion: self miniUpdateLf.
	self assert: res size = 3.
	self assert: res first first = #'Pharo1.1a'.
	self assert: res second first = #'Pharo1.1b'.
	self assert: res third first = #'Pharo1.2a'.
	
	self assert: res first second size = 13.
	self assert: ((res first second) allSatisfy: [:each | each beginsWith: '110']).
	self assert: ((res first second) first = #'11000-Pha-Empty.cs').
	
	self assert: ((res first second) last = #'11012-Pha-ScriptingSystemRemoval.cs').
	


	