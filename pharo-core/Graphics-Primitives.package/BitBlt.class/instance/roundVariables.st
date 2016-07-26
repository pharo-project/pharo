private
roundVariables
	| maxVal minVal |
	maxVal := SmallInteger maxVal.
	minVal := SmallInteger minVal.
	destX := destX asInteger 
		min: maxVal
		max: minVal.
	destY := destY asInteger 
		min: maxVal
		max: minVal.
	width := width asInteger 
		min: maxVal
		max: minVal.
	height := height asInteger 
		min: maxVal
		max: minVal.
	sourceX := sourceX asInteger 
		min: maxVal
		max: minVal.
	sourceY := sourceY asInteger 
		min: maxVal
		max: minVal.
	clipX := clipX asInteger 
		min: maxVal
		max: minVal.
	clipY := clipY asInteger 
		min: maxVal
		max: minVal.
	clipWidth := clipWidth asInteger 
		min: maxVal
		max: minVal.
	clipHeight := clipHeight asInteger 
		min: maxVal
		max: minVal