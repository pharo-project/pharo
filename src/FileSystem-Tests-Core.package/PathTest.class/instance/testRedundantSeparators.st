tests
testRedundantSeparators

	| ref |

	ref := '/a/b/c' asPath / 'foo/'.
	self assert: (ref segments = #('a' 'b' 'c' 'foo')).

	ref := '/a/b/c' asPath / 'foo//'.
	self assert: (ref segments = #('a' 'b' 'c' 'foo')).

	ref := '/a/b/c' asPath / 'foo//..'.
	self assert: (ref segments = #('a' 'b' 'c')).

	ref := '/a/b/c' asPath / '..//foo'.
	self assert: (ref segments = #('a' 'b' 'foo')).

	ref := '/a/b/c' asPath / 'foo//..//bar'.
	self assert: (ref segments = #('a' 'b' 'c' 'bar')).
