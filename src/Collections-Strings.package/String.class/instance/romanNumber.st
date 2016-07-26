converting
romanNumber
	| value v1 v2 |
	value := v1 := v2 := 0.
	self reverseDo:
		[:each |
		v1 := #(1 5 10 50 100 500 1000) at: ('IVXLCDM' indexOf: each).
		v1 >= v2
			ifTrue: [value := value + v1]
			ifFalse: [value := value - v1].
		v2 := v1].
	^ value