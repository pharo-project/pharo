tests
testRedundantSeparators

	| ref |

	ref := '/a/b/c' asPath / 'foo/'.
	self assert: ref segments equals: #('a' 'b' 'c' 'foo').

	ref := '/a/b/c' asPath / 'foo//'.
	self assert: ref segments equals: #('a' 'b' 'c' 'foo').

	ref := '/a/b/c' asPath / 'foo//..'.
	self assert: ref segments equals: #('a' 'b' 'c' 'foo' '..').

	ref := '/a/b/c' asPath / '..//foo'.
	self assert: ref segments equals: #('a' 'b' 'c' '..' 'foo').

	ref := '/a/b/c' asPath / 'foo//..//bar'.
	self assert: ref segments equals: #('a' 'b' 'c' 'foo' '..' 'bar')