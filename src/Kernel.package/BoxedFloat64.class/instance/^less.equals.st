comparing
<= aNumber 
	"Primitive. Compare the receiver with the argument and return true
	if the receiver is less than or equal to the argument. Otherwise return
	false. Fail if the argument is not a Float. Optional. See Object
	documentation whatIsAPrimitive."

	<primitive: 45>
	^ aNumber adaptToFloat: self andCompare: #<=