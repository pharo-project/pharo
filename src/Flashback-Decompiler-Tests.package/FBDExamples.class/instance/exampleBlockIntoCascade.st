examples - blocks
exampleBlockIntoCascade
	| t |
	t := {true.
	true.
	false}.
	^ t
		size;
		collect: [ :aBoolean | 
					| tmp2 |
					tmp2 := true.
					aBoolean and: [ tmp2 ] ];
		yourself