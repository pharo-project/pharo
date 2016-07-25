reading
nextEntryFrom: aStream

	^ reader
		reset;
		on: aStream; 
		next