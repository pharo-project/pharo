instance creation
loadFromPublishUrl: aString
	^ [ self basicLoadFromPublishUrl: aString ] 
		on: Error 
		do: [ nil ]