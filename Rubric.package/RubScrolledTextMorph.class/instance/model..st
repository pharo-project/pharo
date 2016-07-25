accessing
model: aModel
	self textArea model: aModel.
	self setTextWith: self getTextFromModel.
	self setSelection: self getSelectionFromModel.
	aModel announcer when: RubTextAcceptedInModel send: #whenTextAcceptedInModel: to: self.	
	aModel announcer when: RubEditsStateChangedInModel send: #whenEditsStateChangedInModel: to: self.
	aModel announcer when: RubTextSetInModel send: #whenTextSetInModel: to: self.	
	aModel announcer when: RubTextUpdatedInModel send: #whenTextUpdatedInModel: to: self.	
	aModel announcer when: RubPrimarySelectionUpdatedInModel send: #whenPrimarySelectionUpdatedInModel: to: self.
	aModel announcer when: RubCancelEditRequestedInModel send: #whenCancelEditRequestedInModel: to: self.	
	aModel announcer when: RubConfigurationChange send: #whenConfigurationChangedFromModel: to: self.
