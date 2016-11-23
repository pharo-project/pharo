tests
testAsString
	self assert: $a asShortcut asString = 'a'.
	self assert: $A asShortcut asString = 'A'.
	self assert: $a ctrl asString = 'Ctrl + a'.
	self assert: $A ctrl asString = 'Ctrl + A'.
	self assert: $b command asString = 'Cmd + b'.
	self assert: $c shift asString = 'Shift + c'.
	self assert: $d alt asString = 'Alt + d'.
	self assert: $e ctrl command asString =  (KMCtrlModifier new asString ,' + ' , KMCommandModifier new asString,' + e').
	
	
