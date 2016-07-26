private
createClassNamed: className
	| rgClass |
	rgClass := RGClassDefinition named: className.
	rgClass withMetaclass.
	package addClass: rgClass.
	^ rgClass.