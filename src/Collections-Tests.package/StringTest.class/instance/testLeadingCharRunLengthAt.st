tests
testLeadingCharRunLengthAt

self assert: ('test' leadingCharRunLengthAt: 1) = 4.
self assert: ('éà ôüÖ ẞ' leadingCharRunLengthAt: 2) = 7.
self assert: ('123ea slkj jfek' leadingCharRunLengthAt: 1) = 15.
self assert: ('te€st this string' leadingCharRunLengthAt: 1) = 17.
self assert: (' ' leadingCharRunLengthAt: 1) = 1.
self assert: ('te€st this string' leadingCharRunLengthAt: 10) = 8.