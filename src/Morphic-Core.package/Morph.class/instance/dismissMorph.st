meta-actions
dismissMorph
	"This is called from an explicit halo destroy/delete action."

	| w |
	w := self world ifNil:[^self].
	w deleteAllHalos; stopStepping: self.
	self delete