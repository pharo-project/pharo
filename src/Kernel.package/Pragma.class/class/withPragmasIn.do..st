private
withPragmasIn: aClass do: aBlock
	aClass selectorsAndMethodsDo: [ :selector :method | method pragmas do: aBlock ].