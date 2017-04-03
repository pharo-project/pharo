pharo - updates
update60457
	"self new update60457"
	self withUpdateLog: '19892 Socket problem with LatestVM
	https://pharo.fogbugz.com/f/cases/19892

19870 Removing a method using Nautilus while it is present in a message browser raises #ast was sent to nil
	https://pharo.fogbugz.com/f/cases/19870

19860 MC generates duplicated trait methods
	https://pharo.fogbugz.com/f/cases/19860

19899 SettingBrowser>>choosePackagesIn: is not documented at all
	https://pharo.fogbugz.com/f/cases/19899'.
	self loadTogether: self script60457 merge: false.
	
#EpiceaBrowsers asPackage methods select: [ :each | each package ~~ #EpiceaBrowsers asPackage] thenDo: [:each | #EpiceaBrowsers asPackage removeSelector: each selector ofClassName: each methodClass name].
	self flushCaches.
