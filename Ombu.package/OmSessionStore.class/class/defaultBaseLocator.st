accessing
defaultBaseLocator

	^ defaultBaseLocator ifNil: [ 
		defaultBaseLocator := FileLocator localDirectory / 'ombu-sessions' ]