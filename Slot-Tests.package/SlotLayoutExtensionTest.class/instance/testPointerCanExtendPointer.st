tests-valid extensions
testPointerCanExtendPointer

	self should: [ :builder | 
			builder layoutClass: FixedLayout ]
		extendWith: [ :builder | 
			builder layoutClass: FixedLayout ]