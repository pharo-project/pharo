validation
doValidateMethods: selectors on: class
	| association |
	class methods
		ifNotEmpty: [ association := class methods anyOne classBinding.
			class methods do: [ :method | method classBinding == association ] ].

	selectors
		do: [ :selector | 
			self assert: (class canUnderstand: selector).
			self assert: (class >> selector) classBinding value == class ]