private-selector
messagesNameSearch: aString
	"I'm searching for selectors"
	^ self useRegEx
		ifTrue: [ | regex |
			regex := aString asRegex.
			self methodSearch: [ :method| 	regex search: method selector asString ]]
		ifFalse: [
			self methodSearch: (self messageSearchBlockFrom: aString)].