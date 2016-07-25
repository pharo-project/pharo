instance creation
oldValue: oldContent newValue: newValue

	^ self new
		newValue: newValue;
		oldValue: oldContent;
		yourself