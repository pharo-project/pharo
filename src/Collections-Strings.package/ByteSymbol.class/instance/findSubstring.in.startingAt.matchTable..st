comparing
findSubstring: key in: body startingAt: start matchTable: matchTable
	"Answer the index in the string body at which the substring key first occurs, at or beyond start.  The match is determined using matchTable, which can be used to effect, eg, case-insensitive matches.  If no match is found, zero will be returned."
	<primitive: 'primitiveFindSubstring' module: 'MiscPrimitivePlugin'>
	^super findSubstring: key in: body startingAt: start matchTable: matchTable