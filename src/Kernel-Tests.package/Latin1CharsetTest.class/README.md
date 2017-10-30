Tests for the Latin1Charset class.  The invariant is that Latin1Charset is  a subset of Unicode, and therefore all of the methods defined there should
have behaviour consisetent with Unicode.

There may appeaer to be no tests here; that's because this class inherits all of  its tests from AsciiCharset.