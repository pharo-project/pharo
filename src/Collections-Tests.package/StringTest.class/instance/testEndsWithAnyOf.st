tests
testEndsWithAnyOf

self deny: ('éèàôüößäóñíá' endsWithAnyOf: #('ia' 'ñí' '' 'áß')).
self assert: ('éèàôüößäóñíá' endsWithAnyOf: #('test' 'ÉÈÀ' 'äóñíá')).
self assert: ('test' endsWithAnyOf: #('tests' 'tester' 'testing' 't')).
