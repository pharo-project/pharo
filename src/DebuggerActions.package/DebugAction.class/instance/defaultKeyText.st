accessing
defaultKeyText
	"I return a string based on which a default shortcut is created for this action.
	
	I am still present to ensure backwards compatibility.
	The method #defaultKeymap should be overridden instead."
	self flag: 'Enable the deprecation once all usages of #defaultKeyText are removed.'.
	"self
		deprecated: 'Override #defaultKeymap instead to set a keymap object as shortcut.'
		on: '2016-09-05'
		in: #Pharo6."
	
	^ nil
	