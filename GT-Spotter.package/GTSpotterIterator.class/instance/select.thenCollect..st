mimic various enumerating
select: aBlock thenCollect: aBlock2
	^ self select: aBlock thenCollect: aBlock2 as: self defaultItemsClass