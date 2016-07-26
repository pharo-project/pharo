*Collections-arithmetic
max
	^ self inject: self anyOne into: [:max :each | max max: each]