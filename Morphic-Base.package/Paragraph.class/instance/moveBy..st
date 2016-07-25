private
moveBy: delta
	lines do: [:line | line moveBy: delta].
	positionWhenComposed ifNotNil:[
	positionWhenComposed := positionWhenComposed + delta].
	container := container translateBy: delta