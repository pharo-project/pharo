private
handleListMczEntries: request
	^ ZnResponse ok: (ZnEntity html: self repositoryListing)