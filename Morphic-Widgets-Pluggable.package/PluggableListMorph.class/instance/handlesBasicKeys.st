events
handlesBasicKeys
	" if ya don't want the list to automatically handle non-modifier key 
	(excluding shift key) input, return false"
	^ handlesBasicKeys ifNil: [ true ]