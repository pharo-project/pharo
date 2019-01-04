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
	#category : #'Fonts-Infrastructure'
}

{ #category : #accessing }
FontProviderAbstract >> families [
	self subclassResponsibility
]
