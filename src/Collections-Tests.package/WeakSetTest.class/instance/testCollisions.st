tests
testCollisions

	| ws o1 o2 o5 on remember forget |

	"create a weak set"
	ws := WeakSet new: 15.
	
	"select some fractions wanting same place in ws array"
	o1 := (2 to: 100) select: [:i | (ws scanFor: 1 / i) = 1].
	o2 := (2 to: 100) select: [:i | (ws scanFor: 1 / i) = 2].
	o5 := (2 to: 100) select: [:i | (ws scanFor: 1 / i) = 5].
	on := (2 to: 100) select: [:i | (ws scanFor: 1 / i) = (ws array size - 1)].
	
	"Add some fractions to the weak set, and remember a pointer for a few of them"
	remember := OrderedCollection new.
	forget := OrderedCollection new.
	ws add: (remember add: 1 / o1 first).
	ws add: (forget add: 1 / on second).
	ws add: (forget add: 1 / o1 second).
	ws add: (forget add: 1 / o5 second).
	ws add: (forget add: 1 / o2 second).
	ws add: (forget add: 1 / o1 third).
	ws add: (remember add: 1 / o2 first).
	ws add: (forget add: 1 / o5 third).
	ws add: (forget add: 1 / on third).
	ws add: (remember add: 1 / o2 fourth).
	ws add: (remember add: 1 / on first).
	ws add: (remember add: 1 / o5 first).
	
	"forget and reclaim all entries but those in remember"
	forget := nil.
	Smalltalk garbageCollect.
	
	remember do: [:m | self assert: (ws includes: m)].
	ws add: 1/on second.
	remember do: [:m | self assert: (ws includes: m)].
	
	ws add: (remember add: 1 / o1 fourth).
	ws add: (remember add: 1 / on fourth).
	remember remove: (ws remove: (1 / o5 first)).
	remember remove: (ws remove: (1 / on first)).
	remember remove: (ws remove: (1 / o2 first)).
	remember remove: (ws remove: (1 / o1 first)).
	remember do: [:m | self assert: (ws includes: m)].
	ws add: 1/on second.
	ws add: 1/o5 second.
	remember do: [:m | self assert: (ws includes: m)].
	