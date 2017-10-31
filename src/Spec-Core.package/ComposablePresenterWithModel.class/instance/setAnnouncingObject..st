accessing - private
setAnnouncingObject: aDomainObject

	announcingObject := aDomainObject wrappedInValueHolderIfNeeded.
	announcingObject whenChangedDo: [ self modelChanged ].
	