*Collections-arithmetic
min
	^ self inject: self anyOne into: [:min :each | min min: each]