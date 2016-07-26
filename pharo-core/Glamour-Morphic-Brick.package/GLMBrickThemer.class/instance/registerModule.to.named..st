brick-modules
registerModule: aModule to: aThemer named: aString
	"Registers aModule to aThemer. It stores modules in a dictionaty where a key is a selector of my caller method. It allows to customize bricks that are not added to the application (in this case they don't have access to custom themer normaly, because owner is nil) using the same api as added one."
	^ self modules
		at: aString
		ifAbsent: [ aThemer addModule: aModule named: aString ]