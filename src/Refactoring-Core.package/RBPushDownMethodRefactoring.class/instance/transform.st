transforming
transform
	selectors do: [:each | self pushDown: each].
	selectors do: [:each | class removeMethod: each]