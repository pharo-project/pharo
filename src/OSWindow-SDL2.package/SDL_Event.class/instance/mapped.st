accessing
mapped
	^ (EventTypeMap at: self type ifAbsent: [ ^ self unknownEvent ]) fromSdlEvent: self