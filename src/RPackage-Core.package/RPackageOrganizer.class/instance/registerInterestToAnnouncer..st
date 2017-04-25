system integration
registerInterestToAnnouncer: anAnnouncer	
	"There should be only one"
	anAnnouncer unsubscribe: self.
	
	anAnnouncer weak
		when: CategoryAdded send: #systemCategoryAddedActionFrom: to: self;
		when: CategoryRemoved send: #systemCategoryRemovedActionFrom: to: self;
		when: CategoryRenamed send: #systemCategoryRenamedActionFrom: to: self;
		when:  ClassAdded send: #systemClassAddedActionFrom: to: self;
		when:  ClassRecategorized send: #systemClassRecategorizedActionFrom: to: self;
		when:  ClassRemoved send: #systemClassRemovedActionFrom: to: self;
		when:  ClassRenamed send: #systemClassRenamedActionFrom: to: self;
		when:  ClassReorganized send: #systemClassReorganizedActionFrom: to: self;
		when:  MethodAdded send: #systemMethodAddedActionFrom: to: self;
		when:  MethodModified send: #systemMethodModifiedActionFrom: to: self;
		when:  MethodRecategorized send: #systemMethodRecategorizedActionFrom: to: self;
		when:  MethodRemoved send: #systemMethodRemovedActionFrom: to: self;
		when:  ProtocolAdded send: #systemProtocolAddedActionFrom: to: self;
		when:  ProtocolRemoved send: #systemProtocolRemovedActionFrom: to: self.
		
	self flag: #hack. "for decoupling MC"
	self class environment at: #MCWorkingCopy ifPresent: [ 
		anAnnouncer weak
			when: (self class environment at: #MCWorkingCopyCreated) 
				send: #updateAfterNewMCPackageRegistred: 
				to: self;
			when: (self class environment at: #MCWorkingCopyDeleted) 
				send: #updateAfterNewMCPackageUnregistred: 
				to: self	].