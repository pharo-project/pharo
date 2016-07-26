actions
withBindingsDo: aBlock

	self announce: (GLMVariableBindingsAccessRequest new action: aBlock)