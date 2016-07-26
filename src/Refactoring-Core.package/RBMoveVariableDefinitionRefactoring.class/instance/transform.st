transforming
transform
	definingNode removeTemporaryNamed: name.
	blockNodes do: [:each | each body addTemporaryNamed: name].
	class compileTree: parseTree