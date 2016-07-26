private
loadFields
	self isValid 
		ifFalse: [ ^ self error: 'I cannot load face fields because face is not valid.' ].
	self primLoadFields.