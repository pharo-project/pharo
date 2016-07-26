private
hasDimensionRestrictions: aBrick

	^ (self hasMinDimensionRestriction: aBrick) | (self hasMaxDimensionRestriction: aBrick)