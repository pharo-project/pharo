examples - branches
exampleIfNilIfNotNil3
	| tmp1 |
	tmp1:= true.
	^ tmp1 ifNil: [  5 ] ifNotNil: [ 6 ]