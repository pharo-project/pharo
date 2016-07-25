tests - epoch
testDosEpoch

  self useNonUtcTimeZoneDuring: [ | localEpoch |
    localEpoch := '1 January 1980 00:00' asDateAndTime.
    self deny: (DateAndTime dosEpoch = localEpoch) ].
  
  self useTimeZone: 'UTC' during: [ | localEpoch |
    localEpoch := '1 January 1980 00:00' asDateAndTime.
    self assert: DateAndTime dosEpoch equals: localEpoch ].
  
  self assert: DateAndTime dosEpoch equals: '1980-01-01T00:00:00+00:00' asDateAndTime.