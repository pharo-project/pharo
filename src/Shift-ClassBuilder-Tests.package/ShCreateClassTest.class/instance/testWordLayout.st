tests
testWordLayout
	builder name: #SHClassWithWordLayout.
	result := builder
		layoutClass: WordLayout;
		build.

	self validateResult.
	self validateSuperclass: Object.
	self validateSlots: #().

	self assert: result classLayout class equals: WordLayout