private
setSocketStreamParameters: stream
	stream 
		binary;
		shouldSignal: true;
		autoFlush: false;
		bufferSize: self bufferSize;
		timeout: self timeout.
