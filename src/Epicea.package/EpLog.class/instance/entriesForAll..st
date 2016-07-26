accessing
entriesForAll: references

	^ references collect: [ :each | self entryFor: each ]