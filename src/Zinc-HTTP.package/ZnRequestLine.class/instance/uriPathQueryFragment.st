accessing
uriPathQueryFragment
	^ String streamContents: [ :stream | 
			self uri printPathQueryFragmentOn: stream ]