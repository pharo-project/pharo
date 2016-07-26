accessing
string: aString
	(string notNil and: [ aString ~= string ]) ifTrue: [ self invalidateContentLength ].
	string := aString