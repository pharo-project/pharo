list
addSpecialFields: elements

	elements add: (DynamicEyeElement 
			host: self object 
			label: 'ast' 
			value: [ self object ast ]).
			
	elements add: (DynamicEyeElement 
			host: self object 
			label: 'ir' 
			description: [ :value | | string |
				string := value longPrintString.
				string copyFrom: 2 to: string size ]
			value: [ self object ir ]).

	elements add: (DynamicEyeElement 
			host: self object 
			label: 'all bytecodes' 
			value: [ self object symbolic ]).

	elements add: (DynamicEyeElement 
			host: self object 
			label: 'header' 
			value: [ self object headerDescription ]).
			
	self object literals withIndexDo: [ :literal :index|
		elements add: (DynamicEyeElement 
				host: self object 
				label: 'literal ', index asString 
				value: literal) ].
			