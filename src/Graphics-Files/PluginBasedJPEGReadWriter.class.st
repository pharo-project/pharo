"
JPEGReadWriter was replaced by more capable PluginBasedJPEGReadWriter so the old name (PluginBasedJPEGReadWriter) was deprecated.
"
Class {
	#name : 'PluginBasedJPEGReadWriter',
	#superclass : 'JPEGReadWriter',
	#category : 'Graphics-Files',
	#package : 'Graphics-Files'
}

{ #category : 'testing' }
PluginBasedJPEGReadWriter class >> isDeprecated [
	^true
]
