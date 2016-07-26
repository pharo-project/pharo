exception description
defaultAction
    "Result of signal should be the MetacelloProjectSpecForLoad to be used to perform the load. 
	Create a MetacelloProjectSpecForLoad and use the overrideProjectSpec: if you want to supply a different projectSpec"

    ^ MetacelloProjectSpecForLoad new
        projectSpec: self projectSpec;
        yourself