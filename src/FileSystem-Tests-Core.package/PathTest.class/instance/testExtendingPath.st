tests
testExtendingPath

	| ref |

	self should: [ '/a/b' asPath / '' ] raise: Error.
	self should: [ '/a/b' asPath / nil ] raise: Error.

	ref := '/a/b/c' asPath / 'd/e'.
	self assert: ref segments equals: #('a' 'b' 'c' 'd' 'e').

	ref := '/a/b/c' asPath / 'd/e'.
	self assert: ref parent segments equals: #('a' 'b' 'c' 'd').

	ref := '/a/b/c' asPath / '../d'.
	self assert: ref segments equals:  #('a' 'b' 'c' '..' 'd').

	ref := '/a/b/c' asPath / 'd/..'.
	self assert: ref segments equals: #('a' 'b' 'c' 'd' '..').

	ref := '/a/b/c' asPath / 'd/../e'.
	self assert: ref segments equals: #('a' 'b' 'c' 'd' '..' 'e').

	ref := '/a/b/c' asPath / './d'.
	self assert: (ref segments = #('a' 'b' 'c' 'd')).

	ref := '/a/b/c' asPath / 'd/.'.
	self assert: (ref segments = #('a' 'b' 'c' 'd')).

	ref := '/a/b/c' asPath / 'd/./e'.
	self assert: ref segments equals: #('a' 'b' 'c' 'd' 'e').

