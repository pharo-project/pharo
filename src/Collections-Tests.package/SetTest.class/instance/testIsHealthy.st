tests - integrity
testIsHealthy
	"we use associations as keys on purpose, because they changing
	hash depending on the key"
	| a1 a2 set |

	set := Set new.
	[
		a1 := 3 -> nil.
		a2 := nil -> 3.
		set add: a1; add: a2.
		self assert: set isHealthy.
		a1 key: 0.
		a2 key: 0.
		self assert: set isHealthy not
	
	] ensure: [set removeAll]