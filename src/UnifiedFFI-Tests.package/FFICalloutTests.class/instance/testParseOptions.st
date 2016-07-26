tests
testParseOptions
	|generator|
	generator := FFICallout new.
	
	generator parseOptions: #(optStringOrNil).
	self assert: (generator optionAt: #optStringOrNil).
	generator parseOptions: #(- optStringOrNil).
	self deny: (generator optionAt: #optStringOrNil).
	
	"Lets move on to something a bit more advanced..."
	self deny: (generator optionAt: #optNotThere).
	generator parseOptions: #(+ optEncoding: utf8 optStringOrNil  - optNotThere).
	self assert: (generator optionAt: #optEncoding) equals: #utf8.
	self assert: (generator optionAt: #optStringOrNil).
	
	generator parseOptions: #(- optEncoding).
	self deny: (generator optionAt: #optEncoding).
	
	