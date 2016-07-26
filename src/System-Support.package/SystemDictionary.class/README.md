I represent a special dictionary used as global namespace for class names :

	Smalltalk globals classNames.

and for traits too:

	Smalltalk globals traitNames.
	
and a few other globals:

	(Smalltalk globals keys
		reject: [ :k | 
			| g |
			g := Smalltalk globals at: k.
			g isBehavior or: [ g isTrait ] ]) collect: [ :k | k -> (Smalltalk globals at: k) class ].

As the above example let you guess, the global namespace of Smalltalk system is accessed through:

	Smalltalk globals.