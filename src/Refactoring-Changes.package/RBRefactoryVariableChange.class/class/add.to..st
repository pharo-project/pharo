instance creation
add: aVariable to: aBehavior
	"This should only be called on the Add*Change subclasses, but is here so we don't need to copy it to all subclasses"

	^ self new class: aBehavior variable: aVariable