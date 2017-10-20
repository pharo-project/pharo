accessing
newButtons
	"Answer new buttons as appropriate."

	^{self newYesButton. self newNoButton. self newCancelButton isDefault: true}