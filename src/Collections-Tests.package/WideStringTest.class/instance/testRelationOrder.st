tests - relation order
testRelationOrder
	"from johnmci at http://bugs.squeak.org/view.php?id=5331"

	self assert: ('aa' < 'ab').
	self assert: ('aa' <= 'ab').
	self assert: ('aa' <= 'aa').
	self assert: ('ab' > 'aa').
	self assert: ('ab' >= 'aa').
	self assert: ('aa' >= 'aa').
	
	self assert: ('aa' < 'ab' asWideString).
	self assert: ('aa' <= 'ab' asWideString).
	self assert: ('aa' <= 'aa' asWideString).
	self assert: ('ab' > 'aa' asWideString).
	self assert: ('ab' >= 'aa' asWideString).
	self assert: ('aa' >= 'aa' asWideString).
	
	self assert: ('aa' asWideString < 'ab').
	self assert: ('aa' asWideString <= 'ab').
	self assert: ('aa' asWideString <= 'aa').
	self assert: ('ab' asWideString > 'aa').
	self assert: ('ab' asWideString >= 'aa').
	self assert: ('aa' asWideString >= 'aa').
	
	self assert: ('aa' asWideString< 'ab' asWideString).
	self assert: ('aa' asWideString<= 'ab' asWideString).
	self assert: ('aa' asWideString<= 'aa' asWideString).
	self assert: ('ab' asWideString> 'aa' asWideString).
	self assert: ('ab' asWideString >= 'aa' asWideString).
	self assert: ('aa' asWideString>= 'aa' asWideString).