accessing
minExtent
	"Must answer a fixed small size here to
	allow auto orientation to work."
	
	|superMin|
	superMin := super minExtent.
	^(24 max: superMin x) @ (24 max: superMin y)