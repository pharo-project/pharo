utilities
loadVersionInfo: aString
	"If we manage to have a correct string, with some version info, get it there."
	
	| tokens |
	tokens := [ self scanner scan: (ReadStream on: aString) ]
		on: Error
		do: [ #() ].
	^ info := self extractInfoFrom: (self associate: tokens)