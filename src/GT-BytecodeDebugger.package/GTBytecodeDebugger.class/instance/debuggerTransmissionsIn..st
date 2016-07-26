building
debuggerTransmissionsIn: browser

	super debuggerTransmissionsIn: browser.
	browser transmit
		from: #stack port: #selection;
		to: #bytecode;
		andShow: [ :composite :aContext | self bytecodeIn: composite forContext: aContext ]
