Class {
	#name : 'ZdcPluginSSLSessionFinalizationTest',
	#superclass : 'TestCase',
	#instVars : [
		'client',
		'clone'
	],
	#category : 'Zodiac-Tests',
	#package : 'Zodiac-Tests'
}

{ #category : 'running' }
ZdcPluginSSLSessionFinalizationTest >> tearDown [

	client ifNotNil: [ client close ].
	clone ifNotNil: [
		[ clone destroy ]
			on: PrimitiveFailed
			do: [ "I ignore, because if the test works... it has been alread done"
			 ] ].

	super tearDown
]

{ #category : 'tests' }
ZdcPluginSSLSessionFinalizationTest >> testPluginSSLSessionIsCorrectlyFinalized [

	| previousState afterState |

	client := ZnClient new.
	client url: 'https://www.google.com'.
	client method: #GET.
	client execute.
	client response.

	clone := client connection sslSession clone.
		
	previousState := clone serverName.
	
 	client := nil.
	3 timesRepeat: [ Smalltalk garbageCollect ].
	
	afterState := clone serverName.
	
	self deny: previousState equals: afterState. 
]
