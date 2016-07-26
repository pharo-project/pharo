private
downloadTitle
	^ ZnClient new
		url: 'http://tracker.pharo.org/issues/name/', self number asString;
		enforceHttpSuccess: true;
		get.