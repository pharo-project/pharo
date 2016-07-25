scrollbars
createBoxImagesCache
	"Creates the cache to store the arrow forms"
	
	^ LRUCache new
		maximumWeight: 20;
		factory: [ :key |
			self createBoxOfSize: key first color: key second ];
		yourself