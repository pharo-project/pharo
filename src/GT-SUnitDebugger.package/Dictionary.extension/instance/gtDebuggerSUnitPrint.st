*GT-SUnitDebugger
gtDebuggerSUnitPrint

	^ String streamContents: [ :aStream | 
		self keysSortedSafely
				do: [:key | aStream print: key;
						 space;
						 nextPutAll: '->';	
						 space;			
						 print: (self at: key);
						 cr]].