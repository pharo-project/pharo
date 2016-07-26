tests - epoch
testSqueakEpoch

  self useNonUtcTimeZoneDuring: [ | localEpoch |
    localEpoch := '1901-01-01T00:00:00' asDateAndTime.
    self deny: (DateAndTime epoch = localEpoch).
    self deny: (((DateAndTime fromSeconds: 0) offset: 0) = localEpoch) ].
    
  self useTimeZone: 'UTC' during: [ | localEpoch |
    localEpoch := '1901-01-01T00:00:00' asDateAndTime.
    self assert: DateAndTime epoch equals: localEpoch.
    self assert: ((DateAndTime fromSeconds: 0) offset: 0) equals: localEpoch ].
  
  self assert: DateAndTime epoch equals: '1901-01-01T00:00:00+00:00' asDateAndTime. 
  self assert: ((DateAndTime fromSeconds: 0) offset: 0) equals: '1901-01-01T00:00:00+00:00' asDateAndTime.