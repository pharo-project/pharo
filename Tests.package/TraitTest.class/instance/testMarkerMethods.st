testing
testMarkerMethods
	"self debug: #testMarkerMethods"

	self t1 compile: 'm1 self foo bar'.
	self assert: (self t1 >> #m1) markerOrNil isNil.


	self t1 compile: 'm2 self requirement'.
	self assert: (self t1 >> #m2) markerOrNil == #requirement.
	
	self t1 compile: 'm3 ^self requirement'.
	self assert: (self t1 >> #m3) markerOrNil == #requirement.