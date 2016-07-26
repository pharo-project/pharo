tests
testAsString
	self assert: $a ctrl asString = 'Ctrl + A'.
	self assert: $b command asString = 'Cmd + B'.
	self assert: $c shift asString = 'Shift + C'.
	self assert: $d alt asString = 'Alt + D'.
	self assert: $e ctrl command asString =  (KMCtrlModifier new asString ,' + ' , KMCommandModifier new asString,' + E').
	
	
