refreshing
refreshEntryContentModelDeferrer
		
	^ selectionDeferrer ifNil: [
		selectionDeferrer := OmDeferrer 
			send: #refreshEntryContentModel
			to: self
			after: 150 milliSeconds ]