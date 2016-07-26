private
entityWriterOn: stream
	^ ZnEntityWriter new
		headers: self headers;
		stream: stream;
		yourself