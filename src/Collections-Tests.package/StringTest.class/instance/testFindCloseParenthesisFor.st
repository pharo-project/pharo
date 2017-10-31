tests
testFindCloseParenthesisFor

self assert: ('(1 + 3(2 * 9) - 15)' findCloseParenthesisFor: 1) = 19.
self assert: ('(1+(2-3))-3.14159' findCloseParenthesisFor: 1) = 9.
self assert: ('(1+(2-3))-3.14159' findCloseParenthesisFor: 4) = 8.
self assert: ('()' findCloseParenthesisFor: 10) = 3.