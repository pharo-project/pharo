"
Abstract superClass for fontProviders
	examples of possible fontProviders are 
		StrikeFontProvider
		FreeTypeFontProvider
		Win32NativeFontProvider
		
"
Class {
	#name : #FontProviderAbstract,
	#superclass : #Object,
	#category : 'FontInfrastructure'
}

{ #category : #accessing }
FontProviderAbstract >> families [
	self subclassResponsibility
]
