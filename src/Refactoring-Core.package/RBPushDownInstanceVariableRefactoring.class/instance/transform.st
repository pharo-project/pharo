transforming
transform
	class removeInstanceVariable: variableName.
	class subclasses do: [:each | each addInstanceVariable: variableName]