*GT-Inspector
gtInspectorPresentationsIn: composite inContext: aGTInspector
	"This is a utility method that collects all presentations for the current object.
	By default, it simply looks for the #gtInspectorPresentationOrder: pragma.
	The inspector can decice whether or not a presentation should be dispayed.
	Subclasses might want to override it for more special behavior."
	| pragmas |
	pragmas := Pragma
		allNamed: aGTInspector class extensionsPragma
		from: self class
		to: ProtoObject
		sortedUsing: [ :x :y | (x argumentAt: 1) < (y argumentAt: 1) ].
	pragmas := pragmas select: [ :aPragma | aGTInspector shouldDisplayPresentationCreateBy: aPragma method ].
	self gtInspectorPresentationsFromPragmas: pragmas In: composite inContext: aGTInspector