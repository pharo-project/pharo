tests
testCopy

	| copy |

	copy := atPragma copy.

	self deny: atPragma == copy.
	self assert: atPragma method == copy method.
	self assert: atPragma keyword == copy keyword.
	self assert: atPragma arguments == copy arguments.