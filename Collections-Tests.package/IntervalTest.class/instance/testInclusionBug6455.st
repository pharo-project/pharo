tests
testInclusionBug6455
	"This test is about mantis bug http://bugs.squeak.org/view.php?id=6455
	It should work as long as Fuzzy inclusion test feature for Interval of Float is maintained.
	This is a case when tested element is near ones of actual value, but by default.
	Code used to work only in the case of close numbers by excess..."
	
	self assert: ((0 to: Float pi by: Float pi / 100) includes: Float pi * (3/100))