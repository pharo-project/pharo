tests
testComplexChainMatches
	|eCtrl eShift e f|
	
	f := self eventKey: $f.
	eCtrl := self eventKey: $e ctrl: true.
	eShift := self eventKey: $e shift: true.
		
	self assert: (($e ctrl, $e shift, $f) matches: {eCtrl}).
	self assert: (($e ctrl, $e shift, $f) matches: {eCtrl. eShift}).
	self assert: (($e ctrl, $e shift, $f) matches: {eCtrl. eShift. f}).

	self deny: (($e ctrl, $e shift, $f) matchesCompletely: {eCtrl}).
	self deny: (($e ctrl, $e shift, $f) matchesCompletely: {eCtrl. eShift}).
	self assert: (($e ctrl, $e shift, $f) matchesCompletely: {eCtrl. eShift. f}).