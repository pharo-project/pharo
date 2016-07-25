export
exportAllSettings: actions by: groupSize withBasename: aString
	"Export all settings in files. No more than groupSize settings will be exported to the same file. If there are more than groupSize settings, settings will be exported in multiple files (named aString%d.st, with %d a number increasing from 1 to (actions size / groupSize)."
	| index |
	index := 1.
	actions
		groupsOf: groupSize
		atATimeDo: [ :setting | 
			self exportSettings: setting toFileNamed: aString , index printString , '.st'.
			index := index + 1 ].
	self
		exportSettings: (actions last: (actions size rem: groupSize))
		toFileNamed: aString , index printString , '.st'