api-events
whenAboutToStyleChanged: aBlock
	"<api: #event>"
	"Set a block to perform when the about to style block changed"
	
	self whenAboutToStyleBlockChanged: [:block :old :announcement :anAnnouncer | 
		aBlock cull: (block value) cull: old cull: announcement cull: anAnnouncer ]