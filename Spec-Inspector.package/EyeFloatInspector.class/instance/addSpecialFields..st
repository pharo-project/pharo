list
addSpecialFields: elements

	elements add: (DynamicEyeElement 
		host: self object 
		label: 'sign' 
		value: [ self object sign ]).
			
	elements add: (DynamicEyeElement 
		host: self object 
		label: 'significand' 
		value: [ self object significand ]).
			
	elements add: (DynamicEyeElement 
		host: self object 
		label: 'exponent' 
		value: [ self object exponent ]).

	elements add: (DynamicEyeElement
		host: self object
		label: 'binary'
		value: [ self object binaryLiteralString ]).
