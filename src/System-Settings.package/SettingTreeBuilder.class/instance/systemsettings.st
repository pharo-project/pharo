pragmas
systemsettings
	"Process a <systemsettings> pragma"

	<settingPragmaProcessor>
	currentPragma methodClass theNonMetaClass
		perform: currentPragma methodSelector
		with: self