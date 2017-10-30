tests
testTranslateToLowercase

self assert: ('test this string' translateToLowercase) = 'test this string'.
self assert: (' Test THIS sTRing' translateToLowercase) = ' test this string'.
self assert: ('' translateToLowercase) = ''.
self assert: ('ÔÜÖ' translateToLowercase) = 'ôüö'.
self assert: ('123éàôüÖẞ' translateToLowercase) = '123éàôüöẞ'. 