private
evaluatorForSignature: signature "<String|Array>"
	"Search the methods marked with the signature: primtiive for those that match signature.
	 signature is typically a literal Array for the function's C signature, e.g. #(int (*)(int, char *))."
	Pragma withPragmasIn: self class do:
		[:pragma|
		 (pragma key == #signature:
		  and: [(pragma argumentAt: 1) = signature]) ifTrue:
			[^pragma method]].
	self error: 'could not find Callback signature'