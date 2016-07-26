testing
testReferenceResolution
	"RFC 3986 Section 5"
	
	| baseUri specification result succeeded failed |
	baseUri := 'http://a/b/c/d;p?q' asZnUrl.
	specification := {
		"Examples 5.4.1 - Normal" 
		" 'g:h' -> 'g:h'. " "we do not support unknown schemes without //"
		'mailto:john@acme.com' -> 'mailto:john@acme.com'. "this we can do"
		'g' -> 'http://a/b/c/g'.
		'./g' -> 'http://a/b/c/g'.
		'g/' -> 'http://a/b/c/g/'.
		'/g' -> 'http://a/g'.
		'//g' -> 'http://g/'.  "a trailing slash is added automatically"
		'//a.com/assets/img.jpg' -> 'http://a.com/assets/img.jpg'.
		'?y' -> 'http://a/b/c/d;p?y'.
		'g?y' -> 'http://a/b/c/g?y'.
		'#s' -> 'http://a/b/c/d;p?q#s'.
		'g#s' -> 'http://a/b/c/g#s'.
		'g?y#s' -> 'http://a/b/c/g?y#s'.
		';x' -> 'http://a/b/c/;x'.
		'g;x' -> 'http://a/b/c/g;x'.
		'g;x?y#s' -> 'http://a/b/c/g;x?y#s'.
		'' -> 'http://a/b/c/d;p?q'.
		'.' -> 'http://a/b/c/'.
		'./' -> 'http://a/b/c/'.
		'..' -> 'http://a/b/'.
		'../' -> 'http://a/b/'.
		'../g' -> 'http://a/b/g'.
		'../..' -> 'http://a/'.
		'../../' -> 'http://a/'.
		'../../g' -> 'http://a/g'.
		"Examples 5.4.2 - Abnormal" 
		'../../../g' -> 'http://a/g'.
		'../../../../g' -> 'http://a/g'.
		'/./g' -> 'http://a/g'.
		'/../g' -> 'http://a/g'.
		'g.' -> 'http://a/b/c/g.'.
		'.g' -> 'http://a/b/c/.g'.
		'g..' -> 'http://a/b/c/g..'.
		'..g' -> 'http://a/b/c/..g'.
		'./../g' -> 'http://a/b/g'.
		'./g/.' -> 'http://a/b/c/g/'.
		'g/./h' -> 'http://a/b/c/g/h'.
		'g/../h' -> 'http://a/b/c/h'.
		'g;x1/./y' -> 'http://a/b/c/g;x1/y'.
		'g;x1/../y' -> 'http://a/b/c/y'.
		'g?y/./x' -> 'http://a/b/c/g?y/./x'.
		'g?y/../x' -> 'http://a/b/c/g?y/../x'.
		'g#s/./x' -> 'http://a/b/c/g#s/./x'.
		'g#s/../x' -> 'http://a/b/c/g#s/../x'.
		'http://g' -> 'http://g/' "a trailing slash is added automatically"
	}.
	result := specification withIndexCollect: [ :spec :index | | resolved success |
		resolved := baseUri withRelativeReference: spec key.
		success := resolved asString = spec value.
		{ #input -> spec key. #expected -> spec value. #index -> index.
		#resolved -> resolved. #result -> success } asDictionary ].
	succeeded := result select: [ :each | each at: #result ].
	failed := result reject: [ :each | each at: #result ].
	self assert: failed isEmpty