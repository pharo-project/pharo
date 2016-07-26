printing
longPrintOn: stream

	[IRPrinterV2 new
		stream: stream;
		visitNode: self
	] onDNU: #orderNumber do: [:ex | ex resume: ex receiver]