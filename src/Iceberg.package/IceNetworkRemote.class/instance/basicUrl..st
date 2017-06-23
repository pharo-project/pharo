private
basicUrl: aString
	url := (aString endsWith: '.git') 
		ifTrue: [ aString ]
		ifFalse: [ aString, '.git' ]