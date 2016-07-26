*Collections-arithmetic
preMultiplyByMatrix: m
	"Answer m+*self where m is a Matrix."

	m numberOfColumns = self size ifFalse: [self error: 'dimensions do not conform'].
	^ (1 to: m numberOfRows) collect: [ :row | 
			| s |
			s := 0.
			1 to: self size do: [:k | s := (m at: row at: k) * (self at: k) + s].
		s ]