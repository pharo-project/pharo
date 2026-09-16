Class {
	#name : 'StartupPreferencesLoaderTest',
	#superclass : 'TestCase',
	#instVars : [
		'dir',
		'loader'
	],
	#category : 'StartupPreferences-Tests-Startup',
	#package : 'StartupPreferences-Tests',
	#tag : 'Startup'
}

{ #category : 'running' }
StartupPreferencesLoaderTest >> setUp [
	"Only the file-writing path is exercised here, which touches neither the
	 actions dictionary nor the errors collection, so the live singleton needs no
	 saving and restoring."

	super setUp.
	dir := FileSystem memory root / 'prefs'.
	loader := StartupPreferencesLoader default
]

{ #category : 'running' }
StartupPreferencesLoaderTest >> testRewritingWithShorterContentLeavesNoStaleTail [
	"Rewriting an existing script with shorter content leaves exactly the new
	 content. #writeStreamDo: does not truncate, so the tail of the longer previous
	 file would otherwise remain and stop the whole file from parsing at startup,
	 taking every setting in it down (see #19869). Exercised through the private
	 write method because it is the single write point for every #addAtStartupIn*
	 variant, and the public ones all target real folders on disk."

	| long short |
	long := (1 to: 10) collect: [ :i |
		StartupAction name: 'Action ', i printString code: 'Object new' runOnce: true ].
	short := { StartupAction name: 'Action 1' code: 'Object new' runOnce: true }.

	loader addAtStartup: long inDirectory: dir named: 'test.st'.
	loader addAtStartup: short inDirectory: dir named: 'test.st'.

	self
		assert: (dir / 'test.st') contents
		equals: (loader buildStreamFor: short).

	"The previous version is archived outside the '*.st' glob that
	 StartupPreferencesHandler collects, so the folder still holds one startup script."
	self assert: (dir filesMatching: '*.st') size equals: 1
]
