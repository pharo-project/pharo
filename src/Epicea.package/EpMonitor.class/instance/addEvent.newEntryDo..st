private
addEvent: anEvent newEntryDo: aBlock

	[ self 
		addEvent: anEvent
		newEntryDo: aBlock
		triggerReference: self currentTriggerReference ]
			on: OmFileStoreWritingError
			do: [ :error | self handleError: error ]