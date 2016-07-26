private
scheduleFetchFor: packageSpec reference: reference message: message
	self loaderPolicy resetCacheGofer.
	self preLoad: packageSpec.
	(MetacelloDirective loadPackage: packageSpec externalReference: reference loader: self) addTo: self loadDirective.
	self postLoad: packageSpec.
	Transcript
		cr;
		show: message