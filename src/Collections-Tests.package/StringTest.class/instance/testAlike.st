tests
testAlike

self assert: ('test' alike: 'test') >= 8.   "twice second argument size is the best score"
self assert: ('TEST' alike: 'test') >= 8.   "case is ignored"
self assert: ('é è à ô ü ö ß ä ó ñ í á' alike: 'test') = 0. 
self assert: ('é è à ô ü ö' alike: 'é è à ô ü ö ß ä ó ñ í á') >= 10. 
self assert: ('eeeee' alike: 'ééééé') <= 1.  "evaluation based on ascii value"