list
addSpecialFields: elements

	elements add: (DynamicEyeElement 
		host: self object 
		label: 'unicode' 
		description: [ self objectAsUnicodeCodePoint ]
		value: [ self object codePoint ])