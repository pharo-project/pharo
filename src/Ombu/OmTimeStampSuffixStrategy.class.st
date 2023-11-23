"
I attach a timestamp as a suffix to the name (after last _).
"
Class {
	#name : 'OmTimeStampSuffixStrategy',
	#superclass : 'OmSuffixAfterDotStrategy',
	#category : 'Ombu-Strategies',
	#package : 'Ombu',
	#tag : 'Strategies'
}

{ #category : 'accessing' }
OmTimeStampSuffixStrategy >> nextSuffix [
	"There is a hack here because of a difference of behavior between Windows and Unix systems.
	Unix systems gives a time at the nanosecond while windows only does at the millisecond.
	
	To make sure windows generates different store names we wait to be sure that the date and time will be different.
	
	Maybe this will get fixed in the  future? Check https://github.com/pharo-project/pharo/issues/13447 and if it's the case, remove the hack :)
	OmSessionStoreTest>>#testNextStoreName is here to make sure we have no regression while running it on Windows, so if the hack is still needed you will know it."

	Smalltalk os isWindows ifTrue: [ 1 milliSeconds wait ].
	^ DateAndTime now asNanoSeconds printStringHex
]
