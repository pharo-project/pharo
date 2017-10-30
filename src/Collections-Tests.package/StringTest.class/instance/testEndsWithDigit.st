tests
testEndsWithDigit

self deny: ('éèàôüößäóñíá' endsWithDigit).
self assert: ('éèàôüößäóñíá9' endsWithDigit).
self deny: ('99test' endsWithDigit).
self assert: ('  999' endsWithDigit).