testing
testMergedFields
	| request form mergedFields |
	request := ZnRequest post: 'http://host.com/foo?x=1&y=2&x=3'.
	form := ZnApplicationFormUrlEncodedEntity new.
	form
		at: #z put: '100';
		at: #z add: '200';
		at: #y put: '0'.
	request entity: form.
	mergedFields := request mergedFields.
	self assert: mergedFields keys sorted equals: #(x y z).
	self assert: (mergedFields at: #x) sorted equals: #('1' '3').
	self assert: (mergedFields at: #y) sorted equals: #('0' '2').
	self assert: (mergedFields at: #z) sorted equals: #('100' '200')