tests - internet
testWithSqueakLineEndings

	{
		'abc' -> 'abc'.
		'abc', String cr -> ('abc', String cr).
		'abc', String lf -> ('abc', String cr).
		'abc', String crlf -> ('abc', String cr).
		String cr, 'abc' -> (String cr, 'abc').
		String lf, 'abc' -> (String cr, 'abc').
		String crlf, 'abc' -> (String cr, 'abc').
		'abc', String cr, String cr, 'abc' -> ('abc', String cr, String cr, 'abc').
		'abc', String lf, String lf, 'abc' -> ('abc', String cr, String cr, 'abc').
		'abc', String crlf, String crlf, 'abc' -> ('abc', String cr, String cr, 'abc').
		String cr, 'abc', String cr, String crlf, 'abc', String lf -> (String cr, 'abc', String cr, String cr, 'abc', String cr).
		String lf, 'abc', String lf, String crlf, 'abc', String cr -> (String cr, 'abc', String cr, String cr, 'abc', String cr).
	} do: [ :each |
		self assert: each key withSqueakLineEndings = each value ]