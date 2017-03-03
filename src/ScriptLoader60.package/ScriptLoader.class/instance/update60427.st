pharo - updates
update60427
	"self new update60427"
	self withUpdateLog: '18207 Breakpoints still held after a method/class with breakpoints is removed 
	https://pharo.fogbugz.com/f/cases/18207

19669 FastTable jumps to the selected element when its resized
	https://pharo.fogbugz.com/f/cases/19669

19775 Bootstrapped image does not contain class comment of the Context class
	https://pharo.fogbugz.com/f/cases/19775'.
	self loadTogether: self script60427 merge: false.
	self flushCaches.
