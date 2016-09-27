private
buildPragmaMapping
	pragmaMapping := Dictionary new
		at: 'example' put: 'examples';
		at: 'spec' put: 'specs';
		at: 'spec:' put: 'specs';
		at: 'symbolicVersion:' put: 'symbolic versions';
		yourself