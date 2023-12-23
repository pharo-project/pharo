"
**MetacelloLookupProjectSpec** is signalled to allow a handler to substitute a different (equivalent) project spec for fetching or loading purposes
"
Class {
	#name : 'MetacelloLookupProjectSpec',
	#superclass : 'MetacelloScriptProjectSpecNotification',
	#category : 'Metacello-Core-Scripts',
	#package : 'Metacello-Core',
	#tag : 'Scripts'
}

{ #category : 'handlers' }
MetacelloLookupProjectSpec >> handleResolutionFor: aScriptEngine [
    ^ aScriptEngine handleLookupProjectSpec: self
]
