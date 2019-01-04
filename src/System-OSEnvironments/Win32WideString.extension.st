Extension { #name : #Win32WideString }

{ #category : #'*System-OSEnvironments' }
Win32WideString class >> asExternalTypeOn: generator [
	^ FFIExternalObjectType objectClass: self
]
