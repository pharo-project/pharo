settings filters
filterLinkSelectors
  	^ LinkSelectorFilterEnabled ifNil:[ LinkSelectorFilterEnabled := false ]