private
privSaveImageWithCleanup

	"Do not use this method for common image saving. This method is designated for saving the image during the shrinking process in headless images."

	20 timesRepeat: [
		(Delay forMilliseconds: 50) wait.
		Processor yield.
		Smalltalk garbageCollect. ].
 
	[
		self snapshot: true andQuit: true.
	] ifError: [:e |
		| rep |
		rep := FileStream forceNewFileNamed: 'PharoDebug.log'.
		rep nextPutAll: 'Error:'; cr.
		rep nextPutAll: e asString; cr.
  		rep nextPutAll: thisContext stack size asString.
 	  	thisContext stack copy withIndexDo: [:stck :i |
			[ rep nextPutAll: i asString; space; nextPutAll: stck asString; cr] ifError: []].
		rep close. 
		Smalltalk exitFailure ]