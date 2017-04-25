printing
printObject: obj on: s
	[obj printOn: s] on: Exception do: [ :ex | s << '<cannot print object>' ]