tests - testing method dictionary
testWhichSelectorsAccess
	self assert: ((Point whichSelectorsAccess: 'x') includes: #x).
	self deny:  ((Point whichSelectorsAccess: 'y') includes: #x).