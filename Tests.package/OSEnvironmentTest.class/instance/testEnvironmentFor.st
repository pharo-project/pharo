tests
testEnvironmentFor
	
	| compareDictionary |
	
	compareDictionary := ((Smalltalk hasClassNamed: #UnixEnvironment) and: [Smalltalk hasClassNamed: #Win32Environment])
		ifTrue: [ "NativeBoost is present in the image"
			{ 	MacOSPlatform -> #UnixEnvironment. 
	 		  	MacOSXPlatform -> #UnixEnvironment. 
				UnixPlatform -> #UnixEnvironment. 
				Win32Platform -> #Win32Environment  } asDictionary ]
		ifFalse: [ "NativeBoost is NOT present in the image"
			{ 	MacOSPlatform -> #PlatformIndependentEnvironment. 
	 		  	MacOSXPlatform -> #PlatformIndependentEnvironment. 
				UnixPlatform -> #PlatformIndependentEnvironment. 
				Win32Platform -> #PlatformIndependentEnvironment  } asDictionary ].

	compareDictionary keysAndValuesDo: [ :platformClass :envClassName |
		self 
			assert:  (OSEnvironment environmentFor: platformClass new) class name
			equals: envClassName ] 