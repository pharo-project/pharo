tests - match
testMatch
	"from johnmci at http://bugs.squeak.org/view.php?id=5331"
	
	self assert: ('*baz' match: 'mobaz' ).
	self assert: ('*foo#zort' match: 'afoo3zortthenfoo3zort' ).
	self assert: ('*baz' match: 'mobaz' ).
	self assert: ('*foo#zort' match: 'afoo3zortthenfoo3zort' ).
	
	self assert: ('*baz' match: 'mobaz' asWideString).
	self assert: ('*foo#zort' match: 'afoo3zortthenfoo3zort' asWideString).
	self assert: ('*baz' match: 'mobaz' asWideString).
	self assert: ('*foo#zort' match: 'afoo3zortthenfoo3zort' asWideString).
	
	self assert: ('*baz' asWideString match: 'mobaz' ).
	self assert: ('*foo#zort' asWideString match: 'afoo3zortthenfoo3zort' ).
	self assert: ('*baz' asWideString match: 'mobaz' ).
	self assert: ('*foo#zort' asWideString match: 'afoo3zortthenfoo3zort' ).
	
	self assert: ('*baz' asWideString match: 'mobaz' asWideString).
	self assert: ('*foo#zort' asWideString match: 'afoo3zortthenfoo3zort' asWideString).
	self assert: ('*baz' asWideString match: 'mobaz' asWideString).
	self assert: ('*foo#zort' asWideString match: 'afoo3zortthenfoo3zort' asWideString).