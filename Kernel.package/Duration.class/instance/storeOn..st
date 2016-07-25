private
storeOn: aStream
 
 	aStream
 		nextPut: $(;
 		nextPutAll: self className;
 		nextPutAll: ' seconds: ';
 		print: seconds;
 		nextPutAll: ' nanoSeconds: ';
 		print: nanos;
 		nextPut: $).
 