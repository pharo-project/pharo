"
Abstract superClass for fontProviders
	examples of possible fontProviders are 
		StrikeFontProvider
		FreeTypeFontProvider
		Win32NativeFontProvider
		
"
Class {
	#name : 'AbstractFontProvider',
	#superclass : 'Object',
	#category : 'Fonts-Infrastructure-Base',
	#package : 'Fonts-Infrastructure',
	#tag : 'Base'
}

{ #category : 'testing' }
AbstractFontProvider class >> isAbstract [

	 ^self name = #AbstractFontProvider
]

{ #category : 'accessing' }
AbstractFontProvider >> families [

	self subclassResponsibility
]
