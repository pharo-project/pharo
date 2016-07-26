accessing
booleanSetting
	^ booleanMockSetting ifNil: [ booleanMockSetting := true ]