tests
testAsTestMethod
	{((self stubClass >> #one) -> (self stubTestClass >> #testOne)).
	((self stubClass >> #one:) -> (self stubTestClass >> #testOne)).
	((self stubClass >> #two) -> (self stubTestClass >> #testTwo)).
	((self stubClass >> #two:) -> (self stubTestClass >> #testTwo)).
	((self stubClass >> #one:two:) -> (self stubTestClass >> #testOneTwo)).
	((self stubClass >> #oneTwo:) -> (self stubTestClass >> #testOneTwo)).
	((self stubClass >> #untestedOne) -> nil).
	((self stubClass >> #untestedOne:) -> nil).
	((self stubClass >> #untestedTwo) -> nil).
	((self stubClass >> #untestedTwo:) -> nil).
	((self stubClass >> #untestedOne:two:) -> nil).
	((self stubClass >> #untestedOneTwo:) -> nil).
	((self stubClass class >> #one:two:) -> (self stubTestClass >> #testOneTwo)).
	((self stubClass class >> #untestedOne:two:) -> nil).
	((self stubTestClass >> #testOne) -> (self stubTestClass >> #testOne)).
	((self stubTestClass >> #testTwo) -> (self stubTestClass >> #testTwo)).
	((self stubTestClass >> #testOneTwo) -> (self stubTestClass >> #testOneTwo))}
		do: [ :each | self assert: each key asTestMethod == each value ]