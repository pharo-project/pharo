menu - operations
openPreviewToApplySelectedChanges

	| entries previewLog |
	entries := self selectedCodeChanges.
	entries ifEmpty: [ entries := self filteredEntries ].
	entries ifEmpty: [ ^ self inform: 'No changes to preview apply' ].

	previewLog := EpLogBrowserOperationFactory new
		logBrowserModel: self;
		entries: entries;
		errorHandlerBlock: self operationsErrorHandlerBlock;
		newApplyPreviewLog.

	previewLog isEmpty
		ifTrue: [ self inform: 'Preview is empty (no changes to apply in current image)' ]
		ifFalse: [ previewLog browseEvents beApplyMode ]