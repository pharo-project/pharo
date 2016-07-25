accessing
url

	^ '{host}:{port}/{path}' format: {
		#host -> self class host.
		#port -> self class port asString.
		#path -> self path } asDictionary