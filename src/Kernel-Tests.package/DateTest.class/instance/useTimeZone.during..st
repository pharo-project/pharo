helpers
useTimeZone: abbreviation during: aBlock

  | timeZone |
  timeZone := TimeZone abbreviated: abbreviation.
  self restoreLocalTimeZoneAfter: [ 
    DateAndTime localTimeZone: timeZone. 
    aBlock cull: timeZone ].