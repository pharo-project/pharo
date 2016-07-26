private
quickPrint: curve first: aBool
	aBool ifTrue:[Transcript cr].
	Transcript nextPut:$(;
		print: curve start;
		space;
		print: curve via;
		space;
		print: curve end;
		nextPut:$).
	Transcript endEntry.