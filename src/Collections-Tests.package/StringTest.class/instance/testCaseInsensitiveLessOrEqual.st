tests
testCaseInsensitiveLessOrEqual

self assert: ('EEA' caseInsensitiveLessOrEqual: 'fgh').
self deny: ('éèàôüößäóñíá' caseInsensitiveLessOrEqual: 'éèàôüöß').
self assert: ('éèàôüößäóñíá' caseInsensitiveLessOrEqual: 'éèàôüößäóñíá').
self deny: ('ÉÈÀÔÜÖ' caseInsensitiveLessOrEqual: 'èàôüö').
self assert: ('eea' caseInsensitiveLessOrEqual: 'ÉÈÀ').