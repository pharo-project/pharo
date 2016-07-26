halos and balloon help
deleteBalloon
	"If I am showing a balloon, delete it."
	| w |
	w := self world ifNil:[^self].
	w deleteBalloonTarget: self.