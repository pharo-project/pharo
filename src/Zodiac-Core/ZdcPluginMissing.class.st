"
I am ZdcPluginMissing, signaled when a plugin required by Zodiac is missing.

You can find more information on Zodiac's website

	http://zdc.stfx.eu
"
Class {
	#name : #ZdcPluginMissing,
	#superclass : #Error,
	#category : #Zodiac-Core
}

{ #category : #printing }
ZdcPluginMissing >> messageText [
	^ 'SSL/TLS plugin initailization failed (VM plugin missing ? OS libraries missing ?)'
]
