pharo - updates
update60406
	"self new update60406"
	self withUpdateLog: '19731 secondary selection color should be not modified
	https://pharo.fogbugz.com/f/cases/19731

19538 When replacing a slot in class by WeakSlot, it fails to migrate existing instances
	https://pharo.fogbugz.com/f/cases/19538

19603 composition mapping should be initialized on bootstrapped image
	https://pharo.fogbugz.com/f/cases/19603'.
	self loadTogether: self script60406 merge: false.
	
Pharo3DarkTheme withAllSubclassesDo: [:themeClass | themeClass defaultSettings secondarySelectionColor: themeClass basicNew secondarySelectionColor].
	self flushCaches.
