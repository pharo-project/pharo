private
printOn: s
	[ super printOn: s ] on: Error do: [ :ex | s << '![' << ex messageText << ']!' ]