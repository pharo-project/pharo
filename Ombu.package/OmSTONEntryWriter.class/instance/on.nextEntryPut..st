writing
on: aStream nextEntryPut: anEntry 

	writer 
		reset;
		on: aStream;
		nextPut: anEntry.

	aStream cr;cr.