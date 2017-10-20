private
removeRedundantSegments: segments
	"Remove redundant elements ('.' and '') from the supplied segments"

	^segments select:
		[ :segment | segment notEmpty and: [ segment ~= '.' ] ]