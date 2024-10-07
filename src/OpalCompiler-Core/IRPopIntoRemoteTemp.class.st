"
pop into remote temp
"
Class {
	#name : 'IRPopIntoRemoteTemp',
	#superclass : 'IRRemoteTempAccess',
	#category : 'OpalCompiler-Core-IR-Nodes',
	#package : 'OpalCompiler-Core',
	#tag : 'IR-Nodes'
}

{ #category : 'visiting' }
IRPopIntoRemoteTemp >> accept: arg1 [

	^ arg1 visitPopIntoRemoteTemp: self
]
