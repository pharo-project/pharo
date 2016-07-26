editing
style: aText
	self styler ifNotNil: [ :aStyler | aStyler styleInBackgroundProcess: aText ]