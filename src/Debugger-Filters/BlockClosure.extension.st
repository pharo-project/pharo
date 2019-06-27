Extension { #name : #BlockClosure }

{ #category : #'*Debugger-Filters-Extension' }
BlockClosure >> asFilter [
 	^ BlockFilter forBlock: self. 
]
