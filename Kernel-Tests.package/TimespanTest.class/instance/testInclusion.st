tests
testInclusion

	| t1 t2 t3 t4 |
	t1 := timespan start.
	t2 := timespan start + (timespan duration / 2).
	t3 := timespan end.
	t4 := timespan start + (timespan duration).

	self 
		assert: (timespan includes: t1);
		assert: (timespan includes: t2);
		assert: (timespan includes: t3)";
		deny: (timespan includes: t4).
	self
		assert: (timespan includes: (t1 to: t2));
		assert: (timespan includes: (t1 to: t4));
		deny: (timespan includes: (Timespan starting: t2 duration: (timespan duration * 2))).
	self 
		assert: (timespan includesAllOf: { t1. t2. t3 } );
		deny: (timespan includesAllOf: { t1. t2. t3. t4} ).
	self 
		assert: (timespan includesAnyOf: { t1. t2. t3 } );
		deny: (timespan includesAnyOf: { t4 } ).
"