accessing
valuable: aValuable
	"Used when subscriber should be extracted from valuable object"
	self action:  aValuable.
	self subscriber: aValuable receiver.