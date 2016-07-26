tests
testFromDays

	| march18th1627 epochFromDays june2nd1973FromDays march18th1627FromDays january23rd2004FromDays |

	epochFromDays := self dateClass fromDays: 0.
	self assert: epochFromDays equals: (self epoch translateTo: 0).

	june2nd1973FromDays := self dateClass fromDays: (june2nd1973 - self epoch) asDays.
	self assert: june2nd1973FromDays equals: (june2nd1973 translateTo: 0).

	march18th1627 := '18 March 1627' asDate.
	march18th1627FromDays := self dateClass fromDays: (march18th1627 - self epoch) asDays.
	self assert: march18th1627FromDays equals: (march18th1627 translateTo: 0).
	
	january23rd2004FromDays := self dateClass fromDays: 103*365 "years" + 22 "days since Jan 1" + 25 "leap days".
	self assert: january23rd2004FromDays equals: (january23rd2004 translateTo: 0).	