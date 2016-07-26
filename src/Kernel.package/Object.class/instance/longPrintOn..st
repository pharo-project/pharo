printing
longPrintOn: aStream
	"Append to the argument, aStream, the names and values of all 
	of the receiver's instance variables."

	self class allInstVarNames doWithIndex:
		[:title :index |
		aStream nextPutAll: title;
		 nextPut: $:;
		 space;
		 tab;
		 print: (self instVarAt: index);
		 cr]