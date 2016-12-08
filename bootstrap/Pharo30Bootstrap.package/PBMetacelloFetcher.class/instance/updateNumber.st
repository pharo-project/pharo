versioning
updateNumber

	^ (self versionNumber substrings: '.') last allButFirst asInteger asString