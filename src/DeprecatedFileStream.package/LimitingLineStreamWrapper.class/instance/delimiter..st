accessing
delimiter: aString
	"Set limitBlock to check for a delimiting string. Be unlimiting if nil"

	self limitingBlock: (aString caseOf: {
		[nil] -> [[:aLine | false]].
		[''] -> [[:aLine | aLine size = 0]]
	} otherwise: [[:aLine | aLine beginsWith: aString]])
