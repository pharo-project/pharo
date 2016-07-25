tests
testLayoutWithSlotsNotEquals

	self layoutClassesWithSlots do: [:aLayoutClass |
		self 
			assertClassBuiltWith: [:builder | 
				builder 
					layoutClass: aLayoutClass;
					slots: { #field1}  ]
			isNotEqualToClassBuiltWith: [:builder | 
				builder 
					layoutClass: aLayoutClass;
					slots: { #field2}  ]]