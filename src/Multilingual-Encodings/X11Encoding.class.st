"
X11 encoding
"
Class {
	#name : #X11Encoding,
	#superclass : #Object,
	#category : #Multilingual-Encodings
}

{ #category : #accessing }
X11Encoding class >> encoding [

	| enc |
	enc := self getEncoding.
	enc ifNil: [ ^ nil ].
	^ enc asLowercase.
]

{ #category : #accessing }
X11Encoding class >> getEncoding [
	<primitive: 'primGetEncoding' module: 'ImmX11Plugin'>
	^ nil

]

{ #category : #accessing }
X11Encoding class >> getLocaleEncoding [
	<primitive: 'primGetLocaleEncoding' module: 'ImmX11Plugin'>
	^ nil

]

{ #category : #accessing }
X11Encoding class >> getPathEnc [
	<primitive: 'primGetPathEnc' module: 'ImmX11Plugin'>
	^ nil

]

{ #category : #accessing }
X11Encoding class >> getTextEnc [
	<primitive: 'primGetTextEnc' module: 'ImmX11Plugin'>
	^ nil

]

{ #category : #accessing }
X11Encoding class >> getXWinEnc [
	<primitive: 'primGetXWinEnc' module: 'ImmX11Plugin'>
	^ nil

]

{ #category : #accessing }
X11Encoding class >> requestUTF8 [
	<primitive: 'primIsTextEncUTF8' module: 'ImmX11Plugin'>
	^ nil

]

{ #category : #accessing }
X11Encoding class >> requestUTF8: bool [
	<primitive: 'primSetTextEncUTF8' module: 'ImmX11Plugin'>
	^ nil

]

{ #category : #accessing }
X11Encoding class >> setEncoding: encoding [
	<primitive: 'primSetEncoding' module: 'ImmX11Plugin'>
	^ nil

]

{ #category : #accessing }
X11Encoding class >> setEncodingToLocale [
	<primitive: 'primSetEncodingToLocale' module: 'ImmX11Plugin'>
	^ nil

]

{ #category : #accessing }
X11Encoding class >> setPathEnc: encoding [
	<primitive: 'primSetPathEnc' module: 'ImmX11Plugin'>
	^ nil

]

{ #category : #accessing }
X11Encoding class >> setPathEncToLocale [
	<primitive: 'primSetPathEncToLocale' module: 'ImmX11Plugin'>
	^ nil

]

{ #category : #accessing }
X11Encoding class >> setTextEnc: encoding [
	<primitive: 'primSetTextEnc' module: 'ImmX11Plugin'>
	^ nil

]

{ #category : #accessing }
X11Encoding class >> setTextEncToLocale [
	<primitive: 'primSetTextEncToLocale' module: 'ImmX11Plugin'>
	^ nil

]

{ #category : #accessing }
X11Encoding class >> setXWinEnc: encoding [
	<primitive: 'primSetXWinEnc' module: 'ImmX11Plugin'>
	^ nil

]

{ #category : #accessing }
X11Encoding class >> setXWinEncToLocale [
	<primitive: 'primSetXWinEncToLocale' module: 'ImmX11Plugin'>
	^ nil

]

{ #category : #accessing }
X11Encoding class >> useEncoding: encoding [

	self setEncoding: encoding.
	LanguageEnvironment startUp.
	^ self encoding.
]

{ #category : #accessing }
X11Encoding class >> useLocaleEncoding [

	self setEncodingToLocale.
	LanguageEnvironment startUp.
	^ self encoding.
]
