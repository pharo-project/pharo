scripting
withSmalltalkSearch
	self
		searchOn: [:text :each | 
			Smalltalk compiler evaluate: ' | entity each | each := self. entity := each.', text for: each logged: false];
		helpMessage: 'Quick selection field. Given your INPUT, it executes: self select: [:each | INPUT ]'