*Slot
isLegalClassName
	"Answer whether aString is a legal  class name."

	^ [Smalltalk classInstaller validateClassName: self asSymbol] on: Error do: [^ false] 