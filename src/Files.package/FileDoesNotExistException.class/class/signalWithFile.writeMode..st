signalling
signalWithFile: aFile writeMode: writeMode

	^ (self fileName: aFile name)
		readOnly: writeMode not;
		signal