replacing
delete
	sequence ifNil: [self error: 'This node doesn''t have a sequence'].
	sequence remove: self.