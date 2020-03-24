Extension { #name : #KMRepository }

{ #category : #'*Keymapping-Pragmas' }
KMRepository class >> cleanUp [
	self reset
]

{ #category : #'*Keymapping-Pragmas' }
KMRepository class >> reset [
	"Do not reset KMDispatchers instances, it may make the image unusable or force the user to close all the windows."
	
	"TODO: a better reset, allowing the KMDispatcher instances to reload all named, updated, keymaps from the resetted KMRepository."

	self currentWorld setProperty: #kmDispatcher toValue: nil.
	self default: self new.
	KMCategory allSubclasses
		select: [ :c | c isGlobalCategory ]
		thenDo: [ :c | c new installAsGlobalCategory ].
	KMPragmaKeymapBuilder uniqueInstance reset.
]
