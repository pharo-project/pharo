printing
prettyPrintOn: aStream

	(shortcuts select:[ :e | e platform = Smalltalk os platformFamily or: [ e platform = #all ] ])
		do: [ :e | e prettyPrintOn: aStream ]
		separatedBy: [ aStream << ' | ' ]