tests
testUtcOffset
     self assert: (aDateAndTime offset: '0:12:00:00') =  '1970-01-01T12:00:00+12:00' asDateAndTime