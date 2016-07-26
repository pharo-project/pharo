tests
testInclusion
	"Non regression test for another bug of fuzzy inclusion"
	self deny: ((1.0 to: 3.0 by: 1.0 successor) includes: 3.0) description: 'The last element of this Interval is closed to 2'