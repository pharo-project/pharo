building
transmitFromStackToCodeIn: browser
	browser transmit
		from: #stack port: #selection;
		to: #code;
		andShow: [ :composite :aContext | 
			self methodCodeIn: composite forContext: aContext.
			self setUpCodeIn: composite.
			self tearDownCodeIn: composite ]