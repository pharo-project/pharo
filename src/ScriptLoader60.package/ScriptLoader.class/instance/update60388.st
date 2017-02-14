pharo - updates
update60388
	"self new update60388"
	self withUpdateLog: '19695 UUID>>#readHexFrom: accidentally overwrites an inherited method
	https://pharo.fogbugz.com/f/cases/19695

19510 Nautilus Hierarchy view of superclasses does not show all the superclasses
	https://pharo.fogbugz.com/f/cases/19510

19693 GTEventRecorder should be documented
	https://pharo.fogbugz.com/f/cases/19693

19696 Debug world menu still use "Workspace" but open a playgound.
	https://pharo.fogbugz.com/f/cases/19696'.
	self loadTogether: self script60388 merge: false.
	self flushCaches.
