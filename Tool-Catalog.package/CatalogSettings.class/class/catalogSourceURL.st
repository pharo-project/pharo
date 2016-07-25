accessing
catalogSourceURL
	"Return the source URL for retrieving catalog infos"
	
	CatalogSourceURL ifNil: [ CatalogSourceURL := self defaultCatalogSourceURL ].
	^CatalogSourceURL