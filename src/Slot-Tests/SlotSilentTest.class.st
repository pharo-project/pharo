"
I am an abstract class to silence certain parts of the system while running tests in my subclasses.

Currently this class silences the SystemAnnouncer when a test is run. 

"
Class {
	#name : 'SlotSilentTest',
	#superclass : 'SlotClassBuilderTest',
	#instVars : [
		'yetYetAnotherClass'
	],
	#category : 'Slot-Tests-ClassBuilder',
	#package : 'Slot-Tests',
	#tag : 'ClassBuilder'
}

{ #category : 'compiling' }
SlotSilentTest >> compileAccessorsFor: aSlot [
	| readerString writerString |
	readerString := String streamContents: [ :str |
		str
			nextPutAll: aSlot name;
			cr;tab;
			nextPutAll: ' ^';
			nextPutAll: aSlot name.
		 ].
	writerString := String streamContents: [ :str |
		str
			nextPutAll: aSlot name;
			nextPutAll: ': anObject';
			cr;tab;
			nextPutAll: aSlot name;
			nextPutAll: ':= anObject.'.
		].
	aClass compile: readerString classified: 'accessing'.
	aClass compile: writerString classified: 'accessing'
]

{ #category : 'running' }
SlotSilentTest >> runCase [

	self class codeChangeAnnouncer suspendAllWhile: [ super runCase ]
]
