Extension { #name : 'OCBlockNode' }

{ #category : '*Deprecated14' }
OCBlockNode >> hasBlockReturn [

	self deprecated: 'This method is deprecated, don''t use it. Use on your own responsibility.'.

	^ self body lastIsReturn
]
