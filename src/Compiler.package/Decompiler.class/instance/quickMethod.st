private
quickMethod
	| |
	method isReturnSpecial
		ifTrue: [^ constructor codeBlock:
				(Array with: (constTable at: method primitive - 255)) returns: true].
	method isReturnField
		ifTrue: [^ constructor codeBlock:
				(Array with: (constructor codeInst: method returnField)) returns: true].
	self error: 'improper short method'