list
addStackTop: elements

	elements add: (DynamicEyeElement 
			host: self object 
			label: 'stackTop' 
			value: [ self object stackPtr > 0 ifTrue: [ self object top ] ifFalse: [ #emptyStack ] ] ).