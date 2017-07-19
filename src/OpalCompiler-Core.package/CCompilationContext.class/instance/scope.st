accessing
scope
	| newScope |

	newScope := OCClassScope for: class.
	requestor ifNotNil: [
		"the requestor is allowed to manage variables, the workspace is using it to auto-define vars"  
		newScope := (self requestorScopeClass new
			compilationContext: self;
			requestor: requestor) outerScope: newScope].
	^newScope
	