accessing context
setUpPresentationTitle

	^ 'SetUp', 
		(self session setUpMethod methodClass ~= self session testContext methodClass
			ifTrue: [ ' (', self session setUpMethod methodClass name,')' ]
			ifFalse: [ '' ])