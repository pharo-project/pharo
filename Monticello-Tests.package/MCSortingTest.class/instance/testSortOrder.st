tests
testSortOrder
	| aA aAm aB bA bB A B cA bAm cAm |
	aA := self methodNamed: #a class: #A meta: false.
	bA := self methodNamed: #b class: #A meta: false.
	cA := self methodNamed: #c class: #A meta: false.
	aAm := self methodNamed: #a class: #A meta: true.
	bAm := self methodNamed: #b class: #A meta: true.
	cAm := self methodNamed: #c class: #A meta: true.
	aB := self methodNamed: #a class: #B meta: false.
	bB := self methodNamed: #b class: #B meta: false.
	A := self classNamed: #A.
	B := self classNamed: #B.
	self assert: (self sortDefinitions: {aA. aAm. cAm. aB. bAm. bA. bB. A. cA. B})
					= {A. aAm. bAm. cAm. aA. bA. cA. B. aB.  bB}