tests - endswith
testEndsWith
	"from johnmci at http://bugs.squeak.org/view.php?id=5331"
	
	self assert: ('abc' endsWith: 'bc').
	self assert: ('abc' endsWith: 'bc' asWideString).
	self assert: ('abc' asWideString endsWith: 'bc').
	self assert: ('abc' endsWith: 'bX') not .
	self assert: ('abc' endsWith: 'BC') not.
	self assert: ('abc' endsWith: 'BC' asWideString) not .
	self assert: ('ABC' asWideString endsWith: 'bc') not.

