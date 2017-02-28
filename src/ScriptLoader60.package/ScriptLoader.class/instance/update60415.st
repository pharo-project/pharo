pharo - updates
update60415
	"self new update60415"
	self withUpdateLog: '19763 TabManagerMorph do not respect multipleContents mode when tabs are built in background
	https://pharo.fogbugz.com/f/cases/19763

19762 Optimize small Dictionary and Set creation
	https://pharo.fogbugz.com/f/cases/19762

19459 add #empty instance creation method
	https://pharo.fogbugz.com/f/cases/19459

19502 Starting Pharo creates a .ston file in appdata directory
	https://pharo.fogbugz.com/f/cases/19502'.
	self loadTogether: self script60415 merge: false.
	self flushCaches.
