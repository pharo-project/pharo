pharo - updates
update60445
	"self new update60445"
	self withUpdateLog: '19844 remove some trivial duplicated code from TabsExample>>#open
	https://pharo.fogbugz.com/f/cases/19844

19845 add printOn: to ReflectiveMethod
	https://pharo.fogbugz.com/f/cases/19845

19846 Make spotter work with ReflectiveMethods.
	https://pharo.fogbugz.com/f/cases/19846'.
	self loadTogether: self script60445 merge: false.
	self flushCaches.
