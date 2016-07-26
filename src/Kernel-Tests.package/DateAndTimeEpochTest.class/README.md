I represent one of several Sunit test Cases intentended to provide complete coverage  for the Chronology set of classes as part of the external testing. The other Chronology sunit test cases are:
 DateTestCase
 DateAndTimeLeapTestCase,
 DurationTestCase,
 ScheduleTestCase
 TimeStampTestCase
 TimespanDoTestCase, 
 TimespanDoSpanAYearTestCase, 
 TimespanTestCase, 
 YearMonthWeekTestCase.  
These tests attempt to exercise all public and private methods.  Except, they do not explicitly depreciated methods. tlk
My fixtures are:
aDateAndTime = January 01, 1901 midnight (the start of the Squeak epoch) with localTimeZone = Grenwhich Meridian (local offset = 0 hours)
aDuration = 1 day, 2 hours, 3, minutes, 4 seconds and 5 nano seconds.
aTimeZone =  'Epoch Test Time Zone', 'ETZ' , offset: 12 hours, 15 minutes. 