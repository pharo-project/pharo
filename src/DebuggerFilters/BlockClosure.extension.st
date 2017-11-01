Extension { #name : #BlockClosure }

{ #category : #'*DebuggerFilters-Extension' }
BlockClosure >> asFilter [
 	^ BlockFilter forBlock: self. 
]
