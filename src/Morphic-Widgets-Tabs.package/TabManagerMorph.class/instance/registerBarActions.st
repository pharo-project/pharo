initialization
registerBarActions
	
	toolbar 
		when: #tabSelected send: #tabSelected: to: self;
		when: #barDeleted send: #barDeleted: to: self;
		when: #tabsChanged send: #tabsChanged to: self;
		when: #tabRefreshed send: #tabRefreshed: to: self;
		when: #tabAddedToSelection send: #tabAddedToSelection: to: self;
		when: #tabRemovedFromSelection send: #tabRemovedFromSelection: to: self;
		when: #tabResetSelection send: #tabResetSelection: to: self;
		when: #tabEmptyContents send: #tabEmptyContents: to: self