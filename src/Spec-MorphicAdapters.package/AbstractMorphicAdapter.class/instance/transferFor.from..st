drag and drop - private
transferFor: passenger from: source
	^ (self transferBlock cull: passenger cull: self model) buildWithSpec