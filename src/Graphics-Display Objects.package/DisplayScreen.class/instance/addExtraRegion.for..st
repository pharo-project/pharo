displaying
addExtraRegion: aRectangle for: regionDrawer
	"Register the given rectangle as a region which is drawn by the specified region drawer. The region will be excluded from any updates when #forceDamageToScreen: is called. Note that the rectangle is only valid for a single update cycle; once #forceDamageToScreen: has been called, the region drawer and its region are being removed from the list"
	extraRegions ifNil:[extraRegions := #()].
	extraRegions := extraRegions copyWith: (Array with: regionDrawer with: aRectangle).
