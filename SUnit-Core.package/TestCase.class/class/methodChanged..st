initialize - event
methodChanged: anEvent
	"Remove the changed method from the known test results."
	
	| cls sel |
	cls := anEvent methodClass.
	(cls inheritsFrom: TestCase)
		ifFalse: [^ self].
	sel := anEvent selector.
	(sel beginsWith: 'test')
		ifFalse: [^ self].
	self classForTestResult removeFromTestHistory: sel in: cls.
