accessing
highlightingLogic
	"[ :aCanvas :aRectangle ]"
	
	^ self valueOfProperty: #brickHighlightingLogic ifAbsentPut: [
		[ :aCanvas :aRectangle | |rectangle|
			"because font calculates text width wrong"
			rectangle := aRectangle translateBy: ((3@1) negated + self brickBounds origin).
			aCanvas line: rectangle bottomLeft to: rectangle bottomRight width: 1 color: self textColor ] ] 