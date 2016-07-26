scripting
tags: aBlock

	self tagsBlock: aBlock.
	self hasTagsFilter ifFalse:[ self tagsFilter: aBlock ]