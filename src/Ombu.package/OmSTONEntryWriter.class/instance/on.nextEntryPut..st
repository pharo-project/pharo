writing
on: aStream nextEntryPut: anEntry 

	OmCurrentGlobalName
		value: store globalName
		during: [
			writer 
				reset;
				on: aStream;
				nextPut: anEntry.

			aStream cr;cr ]