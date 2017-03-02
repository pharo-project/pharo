pharo - updates
update60421
	"self new update60421"
	self withUpdateLog: '19779 BaselineOfMorphic compiles own version of SystemSettingsPersistence class>>#updateSettingNodes
	https://pharo.fogbugz.com/f/cases/19779

19784 code loss when cancelling during save
	https://pharo.fogbugz.com/f/cases/19784

19776 BaselineOfMorphic and BaselineOfUI both specify Polymorph-TaskbarIcons
	https://pharo.fogbugz.com/f/cases/19776'.
	self loadTogether: self script60421 merge: false.
	
ScriptLoader new unloadPackageNamed: 'Polymorph-TaskbarIcons'.
	self flushCaches.
