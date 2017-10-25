tests - internet
testWithInternetLineEndings

	{
		'abc' -> 'abc'.
		'abc', String cr -> ('abc', String crlf).
		'abc', String lf -> ('abc', String crlf).
		'abc', String crlf -> ('abc', String crlf).
		String cr, 'abc' -> (String crlf, 'abc').
		String lf, 'abc' -> (String crlf, 'abc').
		String crlf, 'abc' -> (String crlf, 'abc').
		'abc', String cr, String cr, 'abc' -> ('abc', String crlf, String crlf, 'abc').
		'abc', String lf, String lf, 'abc' -> ('abc', String crlf, String crlf, 'abc').
		'abc', String crlf, String crlf, 'abc' -> ('abc', String crlf, String crlf, 'abc').
		String cr, 'abc', String cr, String crlf, 'abc', String lf -> (String crlf, 'abc', String crlf, String crlf, 'abc', String crlf).
		String lf, 'abc', String lf, String crlf, 'abc', String cr -> (String crlf, 'abc', String crlf, String crlf, 'abc', String crlf).
	} do: [ :each |
		self assert: each key withInternetLineEndings = each value ]