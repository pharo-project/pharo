pharo - updates
update60484
	"self new update60484"
	self withUpdateLog: '20035 UDPSocketEchoTest testEcho fails on Mac
	https://pharo.fogbugz.com/f/cases/20035

20036 WindowAnnouncementTest is failing on mac
	https://pharo.fogbugz.com/f/cases/20036

20038 GLMPharoScriptMorphicTest and GLMTextMorphicTest are failing on Mac CI
	https://pharo.fogbugz.com/f/cases/20038

20032 RF: compile access to #node as LiteralVariable
	https://pharo.fogbugz.com/f/cases/20032'.
	self loadTogether: self script60484 merge: false.
	self flushCaches.
