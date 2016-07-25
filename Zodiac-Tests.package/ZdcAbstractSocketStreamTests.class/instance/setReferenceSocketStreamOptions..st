private
setReferenceSocketStreamOptions: stream
	stream
		binary;
		shouldSignal: true;
		autoFlush: false;
		bufferSize: self socketBufferSize;
		timeout: self socketStreamTimeout