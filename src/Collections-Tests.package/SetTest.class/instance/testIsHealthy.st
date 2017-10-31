tests - integrity
testIsHealthy
	"we use associations as elements on purpose, because they change their
	hash when we change their keys"

	| a1 a2 set |
	set := self classToBeTested new.
	a1 := 3 -> nil.
	a2 := nil -> 3.
	set
		add: a1;
		add: a2.
	self assert: set isHealthy.
	a1 key: 0.
	a2 key: 0.
	self assert: set isHealthy not