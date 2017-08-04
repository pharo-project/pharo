initialization
initialize
	version := self class formatVersion.
	classes := OrderedCollection new.
	traits := OrderedCollection new.

	extensionMethods := OrderedCollection new.