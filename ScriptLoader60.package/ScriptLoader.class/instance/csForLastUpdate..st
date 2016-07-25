elementary steps
csForLastUpdate: aString
	"Create the changeSet for latest update (preamble, update, postload)"
	
	| updateNumber filename|
	
	updateNumber := self getLatestUpdateNumber.
	filename := updateNumber asString, '-Pha-', aString, '.cs'.
	filename asFileReference 
		ensureDelete;
		writeStreamDo: [ :stream|
			self 
				generateCS: self latestScriptLoaderPackageIdentificationString
				fromUpdate: updateNumber on: stream].
	^ filename