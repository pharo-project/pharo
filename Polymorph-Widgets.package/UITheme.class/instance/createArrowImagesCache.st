scrollbars
createArrowImagesCache
	"Creates the cache to store the arrow forms"

	^ LRUCache new
		maximumWeight: 40;
		factory: [ :key |
			self
				scrollbarArrowOfDirection: key first
				size: key second
				color: key third ];
		yourself