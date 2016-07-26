packing
pack: aGTEventCollector
	^ GTEventBundle new
			url: aGTEventCollector url;
			category: aGTEventCollector category;
			data: (self packCollector: aGTEventCollector);
			yourself