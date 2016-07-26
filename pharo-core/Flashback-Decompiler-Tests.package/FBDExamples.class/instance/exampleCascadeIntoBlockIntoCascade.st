examples - cascades
exampleCascadeIntoBlockIntoCascade
	| t |
	t := {true.
	true.
	false}.
	^ t
		size;
		collect: [ :aBoolean | aBoolean not;value ]