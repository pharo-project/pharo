private
processRelativeReference: reference
	"Parse & resolve the relative reference using myself as base URL, destructively"
	
	| index end |
	reference first = $/ ifTrue: [ self clearPath ].
	(self isFilePath and: [ ('?#' includes: reference first) not ] ) ifTrue: [ self removeLastPathSegment ].
	end := reference size.
	(index := reference indexOf: $#) > 0
		ifTrue: [ 
			self fragment: (self decodePercent: (reference copyFrom: index + 1 to: end)).
			end := index - 1 ].
	(index := reference indexOf: $?) > 0
		ifTrue: [
			self query: (ZnResourceMetaUtils parseQueryFrom: (ReadStream on: reference from: index + 1 to: end)).
			end := index - 1 ].
	self parsePath: (ReadStream on: reference from: 1 to: end).
	(reference = '.' ) | (reference = '..' ) | (reference endsWith: '/.') | (reference endsWith: '/..') 
		ifTrue: [ self closePath ]