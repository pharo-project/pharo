tests
testLayoutWithSlotsEquals

	self layoutClassesWithSlots do: [:aLayoutClass |
		self 
			assertClassBuiltWith: [:builder | 
				builder 
					layoutClass: aLayoutClass;
					slots: { #field1}  ]
			isEqualToClassBuiltWith: [:builder | 
				builder 
					layoutClass: aLayoutClass;
					slots: { #field1}  ].]