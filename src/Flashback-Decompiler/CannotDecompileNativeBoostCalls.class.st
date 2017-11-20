"
Notify that a method can not be decompiled because of a native boost call
"
Class {
	#name : #CannotDecompileNativeBoostCalls,
	#superclass : #Exception,
	#category : #Flashback-Decompiler
}

{ #category : #accessing }
CannotDecompileNativeBoostCalls >> defaultAction [
	UnhandledError signalForException: self
]
