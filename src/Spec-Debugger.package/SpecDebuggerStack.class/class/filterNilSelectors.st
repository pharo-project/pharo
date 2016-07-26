settings filters
filterNilSelectors
  	^ NilSelectorsFilterEnabled ifNil:[ NilSelectorsFilterEnabled := false ]