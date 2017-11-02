"
ZdcPluginSSLSession Tests tests ZdcPluginSSLSession.

"
Class {
	#name : #ZdcPluginSSLSessionTests,
	#superclass : #TestCase,
	#instVars : [
		'session'
	],
	#category : 'Zodiac-Tests'
}

{ #category : #running }
ZdcPluginSSLSessionTests >> setUp [

	session := ZdcPluginSSLSession new
]

{ #category : #running }
ZdcPluginSSLSessionTests >> tearDown [

	session destroy.
	session := nil.
]

{ #category : #tests }
ZdcPluginSSLSessionTests >> testCertificateName [
	
	self assert: session certificateName equals: nil.
	
	session certificateName: 'myCert.pem'.
	
	self assert: session certificateName equals: 'myCert.pem'.
	
	session certificateName: 'otherCert.pem'.
	
	self assert: session certificateName equals: 'otherCert.pem'.
	
	session certificateName: nil.
	
	self assert: session certificateName equals: nil.

]

{ #category : #tests }
ZdcPluginSSLSessionTests >> testServerName [
	
	[ session serverName; serverName: nil ] on: PrimitiveFailed do: [ ^ self skip ].
	
	self assert: session serverName equals: nil.
	
	session serverName: 'pharo.org'.
	
	self assert: session serverName equals: 'pharo.org'.
	
	session serverName: 'smalltalkhub.com'.
	
	self assert: session serverName equals: 'smalltalkhub.com'.
	
	session serverName: nil.
	
	self assert: session serverName equals: nil.

]
