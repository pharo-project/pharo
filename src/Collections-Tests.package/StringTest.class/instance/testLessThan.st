tests
testLessThan

self assert: ('@' < 'A').
self assert: ('abcd' < 'à ô ü').
self assert: ('xyz' < 'ß').
self assert: ('ß t e s t' < 'à ô ü').
self assert: ('' < 'a').
self deny: ('à ô ü' < 'à ô ü').
self deny: ('aaa' < 'AAA').

