"
I return all methods which send #flag: message
"
Class {
	#name : #ClyFlaggingMethodsQuery,
	#superclass : #ClyMethodQuery,
	#category : #'Calypso-SystemPlugins-Flags-Queries'
}

{ #category : #testing }
ClyFlaggingMethodsQuery >> selectsMethod: aMethod [
	^aMethod sendsAnySelectorOf: #(flag:)
]
