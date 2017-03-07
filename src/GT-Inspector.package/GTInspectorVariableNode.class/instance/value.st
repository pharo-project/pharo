accessing
value
	"Answer the object value of this object variable (slot, indexed attribute, computed value).
	Return nil in case there is an error obtaining the value"
	^ [ self rawValue ] on: Error do: [ nil ]