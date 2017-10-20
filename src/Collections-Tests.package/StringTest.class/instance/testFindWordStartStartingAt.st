tests
testFindWordStartStartingAt

self assert: ('test this string' findWordStart: 'th' startingAt: 1) = 6.
self assert: ('test this string' findWordStart: 'st' startingAt: 1) = 11.
self assert: ('test this string' findWordStart: 'st' startingAt: 12) = 0.
self assert: ('test this string' findWordStart: '' startingAt: 1) = 0.
