Class {
	#name : 'InternetConfigurationInitializer',
	#superclass : 'OSPlatformVisitor',
	#category : 'Network-Kernel-Base',
	#package : 'Network-Kernel',
	#tag : 'Base'
}

{ #category : 'visiting' }
InternetConfigurationInitializer >> visitMacOS: aPlatform [

	"Only valid for MacOS right now"
	InternetConfiguration configureHttpProxy
]
