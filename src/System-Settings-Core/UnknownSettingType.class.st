Class {
	#name : 'UnknownSettingType',
	#superclass : 'Error',
	#category : 'System-Settings-Core-Base-New',
	#package : 'System-Settings-Core',
	#tag : 'Base-New'
}

{ #category : 'signalling' }
UnknownSettingType class >> signalType: aSymbol [

	^ self new
		messageText: aSymbol, ' is not part of valid types: ', (', ' join: Setting validTypes);
		signal
]
