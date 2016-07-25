rubric-updating
updateStyle

	self color: self rubricBackgroundColor.
	self rubric borderColor: self rubricBorderColor.
	self rubric borderWidth: self rubricBorderWidth.
	self rubric textColor: self rubricTextColor.
	self rubric font: (LogicalFont familyName: self rubricTextFamilyName pointSize: self rubricTextFontSize).
	self rubric cursor visibleCursorColor: self rubricCursorColor