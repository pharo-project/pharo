html
escapeAttributeValue: string
	"Write string, escaping characters as needed for the value of an attribute"

	string do: [ :each |
		each == $"
			ifTrue: [ stream nextPutAll: '&quot;' ]
			ifFalse: [ stream nextPut: each ] ] 