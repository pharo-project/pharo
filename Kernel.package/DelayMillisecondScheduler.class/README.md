This concrete subclass uses the millisecond primitive & clock, which are based on a 32-bit Integer, rolling over roughly every 6 days.  Special handling for clock rollover is scatter through the implementation.

There are some code duplications with simbling classes for performance reasons.

See parent class comment for more info.