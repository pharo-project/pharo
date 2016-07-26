instance creation
expired
	"Answer an object that is already expired. Used for printing"
	^(self key: nil value: nil)
		expire;
		yourself