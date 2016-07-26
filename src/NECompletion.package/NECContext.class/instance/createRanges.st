private
createRanges
	| parser |
	parser := SHParserST80 new on: controller model.
	ranges := parser 
				rangesIn: source
				classOrMetaClass: theClass
				workspace: controller workspace
				environment: nil.
	"filter out all comments, they don't have any completion/type related information, except the
	one for the current position, as this is used to prevent completion within comments"
	ranges := ranges select: [ :each | each type ~= #comment or: [ each includesPosition: position ] ]