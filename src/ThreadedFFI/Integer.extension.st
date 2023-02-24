Extension { #name : #Integer }

{ #category : #'*ThreadedFFI' }
Integer >> tfPointerAddress [

    ^ExternalAddress new fromInteger: self
]
