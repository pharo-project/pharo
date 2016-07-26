tests-invalid extensions
testWordCannotExtendPointerWithFields

	self shouldnt: [ :builder | 
			builder 
				layoutClass: FixedLayout;
				slots: { #a } ]
		extendWith: [ :builder | 
			builder layoutClass: WordLayout ]