Extension { #name : #Dictionary }

{ #category : #'*GT-SUnitDebugger' }
Dictionary >> gtDebuggerSUnitPrint [

	^ String streamContents: [ :aStream | 
		self keysSortedSafely
				do: [:key | aStream print: key;
						 space;
						 nextPutAll: '->';	
						 space;			
						 print: (self at: key);
						 cr]].
]
