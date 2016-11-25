pharo - updates
update60307
	"self new update60307"
	self withUpdateLog: '19377 Pharo 6 Morph>>#showActions broken
	https://pharo.fogbugz.com/f/cases/19377

19339 MethodAnnouncement should has methodPackage information and provide packagesAffected and protocolsAffected
	https://pharo.fogbugz.com/f/cases/19339

19391 FuelPlatform-Core
	https://pharo.fogbugz.com/f/cases/19391'.
	self loadTogether: self script60307 merge: false.
	
Gofer it smalltalkhubUser: 'Pharo' project: 'Fuel'; package: 'FuelPlatform-Core'; load.
	self flushCaches.
