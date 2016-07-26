as yet unclassified
testClassRemoved
	
	| class |

	class := Object subclass: 'A', UUID new asString36 instanceVariableNames: '' classVariableNames: '' category: package2 name.
	
	package1 mcWorkingCopy modified: false.
	package2 mcWorkingCopy modified: false.
	
	class removeFromSystem.
	
	self assert: package1 mcWorkingCopy modified not.
	self assert: package2 mcWorkingCopy modified.
	
	
	