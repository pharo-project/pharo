tests
testErrorRaised
	| inspector morphsBefore morphsAfter |
	morphsBefore := World submorphs. 
	self instancesToTry do: [ :each | inspector := EyeInspector inspector: each].
	morphsAfter := World submorphs. 
	(morphsAfter copyWithoutAll: morphsBefore) do: [ :morph | morph delete ].