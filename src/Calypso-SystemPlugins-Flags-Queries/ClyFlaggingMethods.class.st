"
I return all methods which send #flag: message
"
Class {
	#name : #ClyFlaggingMethods,
	#superclass : #ClyMethodQuery,
	#category : #'Calypso-SystemPlugins-Flags-Queries'
}

{ #category : #testing }
ClyFlaggingMethods >> selectsMethod: aMethod [
	^aMethod sendsAnySelectorOf: #(flag:)
]
