accessing
siblingSplitters

	^ self owner 
		submorphsSatisfying: [:each | (each isKindOf: self class) 
									and: [self splitsTopAndBottom = each splitsTopAndBottom and: [each ~= self]]]