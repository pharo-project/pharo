as yet unclassified
testClassAdded

	self assert: package1 mcWorkingCopy modified not.
	self assert: package2 mcWorkingCopy modified not.
	
	Object subclass: 'A', UUID new asString36 instanceVariableNames: '' classVariableNames: '' category: package2 name.
	
	self assert: package1 mcWorkingCopy modified not.
	self assert: package2 mcWorkingCopy modified