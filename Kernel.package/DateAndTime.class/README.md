I am DateAndTime.
I represent a point in time or timestamp as defined by ISO 8601. 
I am a Magnitude. 
I have nanosecond precision.
I am TimeZone aware. 
I have zero duration.

  DateAndTime now.
  DateAndTime now asUTC rounded.
  DateAndTime fromString: '1969-07-20T20:17:40.123+02:00'.
  DateAndTime fromString: '1969-07-20T20:17:40Z'.

My implementation uses three SmallIntegers and a Duration:
  julianDayNumber - julian day number (starting at midnight UTC rather than noon GMT).
  seconds - number of seconds since midnight UTC. Always positive, between 0 and 86399.
  nanos	 - the number of nanoseconds since the second. Always positive, between 0 and 999999999.
  offset	- duration from UTC.

The offset is used to print the date and time in a local time zone, but the date and time are handled in UTC internally.
The nanosecond attribute is often zero but it defined for full ISO compliance and is suitable for timestamping.
