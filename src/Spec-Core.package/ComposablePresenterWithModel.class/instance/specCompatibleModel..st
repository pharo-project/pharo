accessing - private
specCompatibleModel: aDomainObject

	specCompatibleModel := aDomainObject asSpecCompatibleModel.
	specCompatibleModel whenChangedDo: [ self modelChanged ].
	