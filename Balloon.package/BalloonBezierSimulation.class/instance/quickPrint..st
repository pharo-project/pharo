private
quickPrint: curve
	Transcript nextPut:$(;
		print: curve start;
		space;
		print: curve via;
		space;
		print: curve end;
		nextPut:$).