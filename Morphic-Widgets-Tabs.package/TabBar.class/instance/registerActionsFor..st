private
registerActionsFor: aTab

	aTab 
		when: #tabSelected send: #tabSelected: to: self;
		when: #tabDeleted send: #tabDeleted: to: self;
		when: #rightKeyPressed send: #rightKeyPressed: to: self;
		when: #leftKeyPressed send: #leftKeyPressed: to: self;
		when: #tabRefreshed send: #tabRefreshed: to: self;
		when: #actionsChanged send: #actionsChanged: to: self;
		when: #tabResetSelection send: #tabResetSelection: to: self;
		when: #tabAddedToSelection send: #tabAddedToSelection: to: self;
		when: #tabRemovedFromSelection send: #tabRemovedFromSelection: to: self;
		when: #tabSelectTo send: #tabSelectTo: to: self;
		when: #tabEmptyContents send: #tabEmptyContents: to: self