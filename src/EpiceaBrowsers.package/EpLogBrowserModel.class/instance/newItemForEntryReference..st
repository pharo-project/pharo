private
newItemForEntryReference: entryReference

	| entry triggers|
	entry := log entryFor: entryReference.
	
	triggers := [ self triggerItemsFor: entry ] 
		on: NotFound 
		do: [ #() ]. "Just hide it... not an esential information"
	
	^ EpEntryItem new
		browser: self;
		entry: entry;
		triggerItems: triggers;
		yourself