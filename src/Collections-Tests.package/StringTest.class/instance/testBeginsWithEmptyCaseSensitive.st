tests
testBeginsWithEmptyCaseSensitive

self deny: ('éèàôüößäóñíá' beginsWithEmpty: 'ß' caseSensitive: true).
self assert: ('éèàôüößäóñíá' beginsWithEmpty: 'éèàô' caseSensitive: true).
self assert: ('éèàôüö' beginsWithEmpty: '' caseSensitive: true).
self deny: ('éèàôüö' beginsWithEmpty: 'ÉÈÀ' caseSensitive: true).
self assert: ('ÉÈÀÔÜÖ' beginsWithEmpty: 'ÉÈÀ' caseSensitive: true).
self assert: ('123test' beginsWithEmpty: '123' caseSensitive: true).
self should: ('aaee' beginsWithEmpty: 'AA' caseSensitive: false).
self should: ('???' beginsWithEmpty: '??' caseSensitive: false).
"self assert: ('ÉÈÀÔÜÖ' beginsWithEmpty: 'éèà' caseSensitive: false)."