private-class
computeListOfClasses: aString
	"Compute in the case I'm searching class names"
		
	^ self useRegEx
		ifTrue: [ | regex |
			regex := aString asRegex.
			self classSearch: [ :class | regex search: class name ]]
		ifFalse: [ 
			self classSearch: [ :class |
				class name includesSubstring: aString caseSensitive: false ]].