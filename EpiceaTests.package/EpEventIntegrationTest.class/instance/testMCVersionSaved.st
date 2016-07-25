tests
testMCVersionSaved

self flag: #todo. "Don't understand why it fails in non-interactive mode (in Jenkins)"

"	| announcements version repository |
	version := self mockVersion.
	repository := MCDictionaryRepository new.
	announcements := OrderedCollection new.

	self assert: (self countLogEventsWith: EpMonticelloVersionSave) equals: 0.

	repository storeVersion: version.

	self assert: (self countLogEventsWith: EpMonticelloVersionSave) equals: 1.
"