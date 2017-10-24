tests
testAllRangesOfSubstring

self assert: ('aabbccbb' allRangesOfSubstring: 'bb' ) = {(3 to: 4). (7 to: 8)}.
self assert: ('é è à ô ü ö ß ä ó ñ í á' allRangesOfSubstring: 'ö ß ä') = {(11 to: 15)}.
self assert: ('test' allRangesOfSubstring: '') = #().