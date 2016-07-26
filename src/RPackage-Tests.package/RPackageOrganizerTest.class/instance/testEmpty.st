tests
testEmpty
	"self debug: #testEmpty"
	self assert: self packageOrganizer packageNames size = 1.
	self assert: (self packageOrganizer packageNames includes: self packageClass defaultPackageName)
