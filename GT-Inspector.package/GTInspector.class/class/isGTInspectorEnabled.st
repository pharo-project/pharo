settings
isGTInspectorEnabled

	^ GTInspectorEnabledStatus ifNil: [ GTInspectorEnabledStatus := Smalltalk tools inspector = self ]