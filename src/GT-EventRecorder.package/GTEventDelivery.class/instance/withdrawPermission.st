deactivation
withdrawPermission
	mutex critical: [ permission := false ]