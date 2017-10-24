tests
testTranslateToUppercase

self assert: ('test this string' translateToUppercase) = 'TEST THIS STRING'.
self assert: (' Test THIS sTRing' translateToUppercase) = ' TEST THIS STRING'.
self assert: ('' translateToUppercase) = ''.
self assert: ('ÔÜÖ' translateToUppercase) = 'ÔÜÖ'.
self assert: ('123éàôüÖẞ' translateToUppercase) = '123ÉÀÔÜÖẞ'.