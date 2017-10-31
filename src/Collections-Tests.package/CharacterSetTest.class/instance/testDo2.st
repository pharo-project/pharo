tests - iterating
testDo2
	| setWithAtoE result |
	setWithAtoE := self classToBeTested withAll: ($A to: $E).
	result := Set new.
	setWithAtoE do: [ :each | result add: each].
	self assert: result size equals: setWithAtoE size.
	result do: [ :each | self assert: (setWithAtoE includes: each) ]