tests
testAsDate

self assert: ('Jan 1 2015' asDate asString) = '1 January 2015'. 
self assert: ('1/1/2015' asDate asString) = '1 January 2015'.
self assert: ('1 1 1' asDate asString) = '1 January 2001'.
self assert: ('1 J 1' asDate asString) = '1 January 2001'.
self should: ['' asDate] raise: Error.
self should: ['1234' asDate] raise: Error. 