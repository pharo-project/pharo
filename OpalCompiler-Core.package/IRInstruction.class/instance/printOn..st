printing
printOn: aStream
	IRPrinterV2 new
			stream: aStream;
			visitNode: self