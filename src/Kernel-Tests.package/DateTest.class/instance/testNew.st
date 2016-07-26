tests
testNew
	| instance epoch |
	instance := self dateClass new.
	epoch := self epoch.
	"We have to be careful, since #new creates a Date for midnight in the local timezone, whereas #epoch is based on a fixed time in UTC"
	instance offset negative 
		ifTrue: [ epoch := epoch - 1 day ]. 
	self
		assert: instance
		equals: epoch.